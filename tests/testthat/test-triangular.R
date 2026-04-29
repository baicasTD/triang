test_that("dtriang covers all branches", {

  # fuera del rango
  expect_equal(dtriang(-1, 0, 10, 5), 0)
  expect_equal(dtriang(11, 0, 10, 5), 0)

  # extremos
  expect_equal(dtriang(0, 0, 10, 5), 0)
  expect_equal(dtriang(10, 0, 10, 5), 0)

  # tramo creciente
  expect_equal(dtriang(2, 0, 10, 5),
               2*(2-0)/((10-0)*(5-0)))

  # modo
  expect_equal(dtriang(5, 0, 10, 5), 0.2)

  # tramo decreciente
  expect_equal(dtriang(8, 0, 10, 5),
               2*(10-8)/((10-0)*(10-5)))
})

test_that("ptriang covers all branches", {

  # fuera
  expect_equal(ptriang(-1, 0, 10, 5), 0)
  expect_equal(ptriang(11, 0, 10, 5), 1)

  # extremos
  expect_equal(ptriang(0, 0, 10, 5), 0)
  expect_equal(ptriang(10, 0, 10, 5), 1)

  # tramo creciente
  expect_equal(ptriang(2, 0, 10, 5),
               (2^2)/(10*5))

  # tramo decreciente
  expect_equal(ptriang(8, 0, 10, 5),
               1 - ((10-8)^2)/(10*(10-5)))
})

test_that("qtriang full coverage", {

  expect_equal(qtriang(0, 0, 10, 5), 0)
  expect_equal(qtriang(1, 0, 10, 5), 10)

  expect_true(qtriang(0.2, 0, 10, 5) < 5)
  expect_true(qtriang(0.8, 0, 10, 5) > 5)
})

test_that("error handling", {
  expect_error(dtriang(1, 5, 5, 5))       # min >= max
  expect_error(dtriang(1, 0, 10, 15))     # mode fuera
  expect_error(qtriang(-0.1, 0, 10, 5))   # p < 0
  expect_error(qtriang(1.1, 0, 10, 5))    # p > 1
})

test_that("vectorization fully tested", {
  x <- c(-1, 2, 5, 8, 11)
  y <- dtriang(x, 0, 10, 5)
  expect_equal(length(y), 5)
})
