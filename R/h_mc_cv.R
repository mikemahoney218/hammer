#' Monte Carlo Cross-Validation
#'
#' One resample of Monte Carlo cross-validation takes a random sample (without
#'  replacement) of the original data set to be used for analysis. All other
#'  data points are added to the assessment set.
#'
#' @details
#'
#'  With a `strata` argument, the random sampling is conducted
#'  *within the stratification variable*. This can help ensure that the
#'  resamples have equivalent proportions as the original data set. For
#'  a categorical variable, sampling is conducted separately within each class.
#'  For a numeric stratification variable, `strata` is binned into quartiles,
#'  which are then used to stratify. Strata below 10% of the total are
#'  pooled together; see [make_strata()] for more details.
#'
#' @inheritParams rsample::vfold_cv
#' @inheritParams rsample::make_strata
#' @param prop The proportion of data to be retained for modeling/analysis.
#' @param times The number of times to repeat the sampling.
#' @param weights A variable in `data` (single character or name) used to
#' assign probabilities of any observation being selected for the analysis set.
#' When not `NULL`, higher weights are more likely to be sampled for each
#' analysis set. When `NULL`, all observations are equally likely to be sampled.
#' @export
#' @return An tibble with classes `weighted_mc_cv`, `mc_cv`, `rset`, `tbl_df`,
#'  `tbl`, and `data.frame`. The results include a column for the data split
#'  objects and a column called `id` that has a character string with the
#'  resample identifier.
#' @examplesIf rlang::is_installed("modeldata")
#' h_mc_cv(mtcars, times = 2)
#' h_mc_cv(mtcars, prop = .5, times = 2)
#'
#' library(purrr)
#' data(wa_churn, package = "modeldata")
#'
#' set.seed(13)
#' resample1 <- h_mc_cv(wa_churn, times = 3, prop = .5)
#' map_dbl(
#'   resample1$splits,
#'   function(x) {
#'     dat <- as.data.frame(x)$churn
#'     mean(dat == "Yes")
#'   }
#' )
#'
#' set.seed(13)
#' resample2 <- h_mc_cv(wa_churn, strata = churn, times = 3, prop = .5)
#' map_dbl(
#'   resample2$splits,
#'   function(x) {
#'     dat <- as.data.frame(x)$churn
#'     mean(dat == "Yes")
#'   }
#' )
#'
#' set.seed(13)
#' resample3 <- h_mc_cv(wa_churn, strata = tenure, breaks = 6, times = 3, prop = .5)
#' map_dbl(
#'   resample3$splits,
#'   function(x) {
#'     dat <- as.data.frame(x)$churn
#'     mean(dat == "Yes")
#'   }
#' )
#' @export
h_mc_cv <- function(data, prop = 3 / 4, times = 25,
                  strata = NULL, breaks = 4, pool = 0.1, ..., weights = NULL) {
  rlang::check_dots_empty()

  if (!missing(strata)) {
    strata <- tidyselect::vars_select(names(data), !! rlang::enquo(strata))
    if (length(strata) == 0) strata <- NULL
  }

  if (!missing(weights)) {
    weights <- tidyselect::vars_select(names(data), !! rlang::enquo(weights))
  }

  if (is.null(weights) || length(weights) == 0) {
    weights <- NULL
    weights_vec <- NULL
  } else {
    weights_vec <- getElement(data, weights)
  }

  strata_check(strata, data)

  split_objs <-
    mc_splits(
      data = data,
      prop = prop,
      times = times,
      strata = strata,
      breaks = breaks,
      pool = pool,
      weights = weights_vec
    )

  ## We remove the holdout indices since it will save space and we can
  ## derive them later when they are needed.

  split_objs$splits <- purrr::map(split_objs$splits, rm_out)

  if (!is.null(strata)) names(strata) <- NULL
  if (!is.null(weights)) names(weights) <- NULL
  mc_att <- list(
    prop = prop,
    times = times,
    strata = strata,
    breaks = breaks,
    pool = pool,
    weights = weights
  )

  rsample::new_rset(
    splits = split_objs$splits,
    ids = split_objs$id,
    attrib = mc_att,
    subclass = c("weighted_mc_cv", "mc_cv", "rset")
  )
}

# Get the indices of the assessment set from the analysis set
mc_complement <- function(ind, n) {
  list(
    analysis = ind,
    assessment = setdiff(1:n, ind)
  )
}


mc_splits <- function(data, prop = 3 / 4, times = 25,
                      strata = NULL, breaks = 4, pool = 0.1, weights = NULL) {
  if (!is.numeric(prop) | prop >= 1 | prop <= 0) {
    rlang::abort("`prop` must be a number on (0, 1).")
  }

  n <- nrow(data)
  if (is.null(strata)) {
    indices <- purrr::map(
      rep(n, times),
      sample,
      size = floor(n * prop),
      prob = weights
    )
  } else {
    stratas <- tibble::tibble(
      idx = 1:n,
      strata = rsample::make_strata(getElement(data, strata),
                           breaks = breaks,
                           pool = pool
      ),
      weights = weights
    )
    stratas <- split_unnamed(stratas, stratas$strata)
    stratas <-
      purrr::list_rbind(
        purrr::map(stratas, strat_sample, prop = prop, times = times)
      )

    indices <- split_unnamed(stratas$idx, stratas$rs_id)
  }
  indices <- lapply(indices, mc_complement, n = n)
  split_objs <-
    purrr::map(indices, rsample::make_splits, data = data, class = "mc_split")
  list(
    splits = split_objs,
    id = names0(length(split_objs), "Resample")
  )
}

strat_sample <- function(x, prop, times, ...) {
  n <- nrow(x)
  if ("weights" %in% names(x)) {
    weights <- x[["weights"]]
  } else {
    weights <- NULL
  }
  idx <- purrr::map(rep(n, times), sample, size = floor(n * prop), prob = weights, ...)
  out <- purrr::list_rbind(
    purrr::map(idx, function(ind, x) x[sort(ind), "idx"], x = x)
  )

  out$rs_id <- rep(1:times, each = floor(n * prop))
  out
}

strata_check <- function(strata, data) {
  if (!is.null(strata)) {
    if (!is.character(strata) | length(strata) != 1) {
      rlang::abort("`strata` should be a single name or character value.")
    }
    if (inherits(data[, strata], "Surv")) {
      rlang::abort("`strata` cannot be a `Surv` object. Use the time or event variable directly.")
    }
    if (!(strata %in% names(data))) {
      rlang::abort(strata, " is not in `data`.")
    }
  }
  invisible(NULL)
}

split_unnamed <- function(x, f) {
  out <- split(x, f)
  unname(out)
}

rm_out <- function(x) {
  x$out_id <- NA
  x
}

dim_rset <- function(x, ...) {
  rlang::check_dots_empty()
  dims <- purrr::map(x$splits, dim)
  dims <- do.call("rbind", dims)
  dims <- tibble::as_tibble(dims)
  id_cols <- grep("(^id$)|(^id[1-9]$)", colnames(x), value = TRUE)
  for (i in seq_along(id_cols)) {
    dims[id_cols[i]] <- getElement(x, id_cols[i])
  }
  dims
}

names0 <- function(num, prefix = "x") {
  if (num == 0L) {
    return(character())
  }
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}
