test_that("setting weights works", {
  expect_true(
    all(
      vapply(
        h_mc_cv(
          tibble::tibble(x = 1:10, y = c(rep(0, 9), 1)),
          weights = y,
          prop = 0.1
        )$splits,
        function(x) rsample::analysis(x)$x == 10,
        logical(1)
      )
    )
  )
})

test_that("setting strata and weights works", {
  expect_true(
    all(
      vapply(
        h_mc_cv(
          tibble::tibble(x = 1:10, y = c(rep(0, 8), 1, 1), z = rep(1:2, 5)),
          weights = y,
          strata = z,
          prop = 0.2
        )$splits,
        function(x) rsample::analysis(x)$x  == c(9, 10),
        logical(2)
      )
    )
  )
})
