testthat::test_that("dtriang works", {
  testthat::expect_equal(dtriang(5, 0, 10, 5), 0.2)
  testthat::expect_equal(dtriang(-1, 0, 10, 5), 0)
})

testthat::test_that("ptriang boundaries", {
  testthat::expect_equal(ptriang(0, 0, 10, 5), 0)
  testthat::expect_equal(ptriang(10, 0, 10, 5), 1)
})

testthat::test_that("quantile inversion", {
  p <- 0.4
  testthat::expect_equal(
    ptriang(qtriang(p, 0, 10, 5), 0, 10, 5),
    p,
    tolerance = 1e-6
  )
})

testthat::test_that("random generation", {
  x <- rtriang(5, 0, 10, 5)
  testthat::expect_equal(length(x), 5)
})

testthat::test_that("error handling", {
  testthat::expect_error(dtriang(1, 10, 0, 5))
  testthat::expect_error(qtriang(-0.1, 0, 10, 5))
})
