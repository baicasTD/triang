test_that("dtriang full coverage", {

  expect_equal(dtriang(-1, 0, 10, 5), 0)
  expect_equal(dtriang(11, 0, 10, 5), 0)

  expect_equal(dtriang(0, 0, 10, 5), 0)
  expect_equal(dtriang(10, 0, 10, 5), 0)

  expect_equal(dtriang(2, 0, 10, 5),
               2*(2-0)/((10-0)*(5-0)))

  expect_equal(dtriang(5, 0, 10, 5), 0.2)

  expect_equal(dtriang(8, 0, 10, 5),
               2*(10-8)/((10-0)*(10-5)))
})

test_that("ptriang full coverage", {

  expect_equal(ptriang(-1, 0, 10, 5), 0)
  expect_equal(ptriang(11, 0, 10, 5), 1)

  expect_equal(ptriang(0, 0, 10, 5), 0)
  expect_equal(ptriang(10, 0, 10, 5), 1)

  expect_equal(ptriang(2, 0, 10, 5),
               (2^2)/(10*5))

  expect_equal(ptriang(8, 0, 10, 5),
               1 - ((10-8)^2)/(10*(10-5)))
})

test_that("qtriang full coverage", {

  expect_equal(qtriang(0, 0, 10, 5), 0)
  expect_equal(qtriang(1, 0, 10, 5), 10)

  expect_true(qtriang(0.2, 0, 10, 5) < 5)
  expect_true(qtriang(0.8, 0, 10, 5) > 5)
})

test_that("rtriang full coverage", {

  x <- rtriang(100, 0, 10, 5)
  expect_equal(length(x), 100)
  expect_true(all(x >= 0 & x <= 10))
})

test_that("error handling", {

  expect_error(dtriang(1, 5, 5, 5))
  expect_error(dtriang(1, 0, 10, 15))

  expect_error(ptriang(1, 5, 5, 5))

  expect_error(qtriang(-0.1, 0, 10, 5))
  expect_error(qtriang(1.1, 0, 10, 5))

  expect_error(rtriang(0, 0, 10, 5))
})

test_that("vectorization", {

  x <- c(-1, 0, 2, 5, 8, 10, 11)
  res <- dtriang(x, 0, 10, 5)

  expect_equal(length(res), length(x))
})

test_that("extra validation coverage", {

  expect_error(ptriang(5, 10, 0, 5))   # min >= max
  expect_error(ptriang(5, 0, 10, -1))  # mode fuera

  expect_error(qtriang(0.5, 10, 0, 5))  # parámetros incorrectos

})

test_that("boundary transitions", {

  # justo en el modo
  expect_equal(ptriang(5, 0, 10, 5), 0.5)

  # justo antes del modo
  expect_true(ptriang(4.999, 0, 10, 5) < 0.5)

  # justo después del modo
  expect_true(ptriang(5.001, 0, 10, 5) > 0.5)
})

test_that("qtriang transition point", {

  pc <- (5 - 0)/(10 - 0)

  expect_equal(qtriang(pc, 0, 10, 5), 5)
})

test_that("ptriang extra values", {

  expect_true(ptriang(1, 0, 10, 5) > 0)
  expect_true(ptriang(9, 0, 10, 5) < 1)

})


