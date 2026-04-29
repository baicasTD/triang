
test_that("dtriang full coverage", {
  expect_equal(dtriang(-1, 0, 10, 5), 0)
  expect_equal(dtriang(0, 0, 10, 5), 0)
  expect_true(dtriang(2, 0, 10, 5) > 0)
  expect_equal(dtriang(5, 0, 10, 5), 0.2)
  expect_true(dtriang(8, 0, 10, 5) > 0)
  expect_equal(dtriang(10, 0, 10, 5), 0)
  expect_equal(dtriang(11, 0, 10, 5), 0)
})

test_that("ptriang full coverage", {
  expect_equal(ptriang(-1, 0, 10, 5), 0)
  expect_equal(ptriang(0, 0, 10, 5), 0)
  expect_true(ptriang(3, 0, 10, 5) > 0)
  expect_equal(ptriang(10, 0, 10, 5), 1)
  expect_equal(ptriang(11, 0, 10, 5), 1)
})

test_that("qtriang edges", {
  expect_equal(qtriang(0, 0, 10, 5), 0)
  expect_equal(qtriang(1, 0, 10, 5), 10)
})

test_that("vectorization works", {
  x <- c(1, 5, 9)
  expect_equal(length(dtriang(x, 0, 10, 5)), 3)
})

test_that("additional errors", {
  expect_error(dtriang(1, 1, 1, 1))
  expect_error(ptriang(1, 10, 0, 5))
})
