
rtnRandomSample <- function(v, n, ...) {
  return(runif(n, min = min(v), max = max(v)))
}


test_that("boolean ops throw a warning once", {
  xval <- 10
  xlci <- 1
  xuci <- 100
  x <- ValueWithUncertainty(xlci, xval, xuci)

  expect_warning(expect_equal(!x, !xval))
  expect_silent(expect_equal(!x, !xval))
  options(ValueWithUncertainty.warn.bool = TRUE)
  expect_warning(expect_equal(x & x, xval & xval))
  expect_silent(expect_equal(x & x, xval & xval))
  options(ValueWithUncertainty.warn.bool = TRUE)
  expect_warning(expect_equal(x | x, xval | xval))
  expect_silent(expect_equal(x | x, xval | xval))
  options(ValueWithUncertainty.warn.bool = TRUE)
  expect_warning(expect_equal(x == x, xval == xval))
  expect_silent(expect_equal(x == x, xval == xval))
  options(ValueWithUncertainty.warn.bool = TRUE)
  expect_warning(expect_equal(x != x, xval != xval))
  expect_silent(expect_equal(x != x, xval != xval))
  options(ValueWithUncertainty.warn.bool = TRUE)
  expect_warning(expect_equal(x < x, xval < xval))
  expect_silent(expect_equal(x < x, xval < xval))
  options(ValueWithUncertainty.warn.bool = TRUE)
  expect_warning(expect_equal(x > x, xval > xval))
  expect_silent(expect_equal(x > x, xval > xval))
  options(ValueWithUncertainty.warn.bool = TRUE)
  expect_warning(expect_equal(x <= x, xval <= xval))
  expect_silent(expect_equal(x <= x, xval <= xval))
  options(ValueWithUncertainty.warn.bool = TRUE)
  expect_warning(expect_equal(x >= x, xval >= xval))
  expect_silent(expect_equal(x >= x, xval >= xval))
})

test_that("ops with numerics don't throw a warning", {
  xval <- 10
  xlci <- 1
  xuci <- 100
  x <- ValueWithUncertainty(xlci, xval, xuci)

  expect_silent(1 + x)
  expect_silent(1 + x)
  expect_silent(x + 1)
  expect_silent(x + 1)
})

test_that("ops work properly", {
  xval <- 10
  xlci <- -10
  xuci <- 100
  x <- ValueWithUncertainty(xlci, xval, xuci)
  y <- ValueWithUncertainty(xlci, xval, xuci)

  expect_equal(as.numeric(x), xval)
  expect_equal(+x, xval)
  expect_equal(x + ValueWithUncertainty(xlci, xval, xuci), 2 * xval)
  expect_equal(x + x, 2 * xval)
  expect_equal(x + y, 2 * xval)
  expect_equal(-x, -xval)
  expect_equal(x - ValueWithUncertainty(xlci, 1, xuci), xval - 1)
  expect_equal(x - x, 0)
  expect_equal(x - y, 0)
  expect_equal(ValueWithUncertainty(xlci, 0, xuci) * x, 0)
  expect_equal(ValueWithUncertainty(xlci, 2, xuci) * x, 2 * xval)
  expect_equal(x * x, xval * xval)
  expect_equal(x * y, xval * xval)
  expect_equal(x^ValueWithUncertainty(xlci, 2, xuci), xval * xval)
  expect_equal(x / ValueWithUncertainty(xlci, 2, xuci), xval / 2)
  expect_equal(x / x, 1)
  expect_equal(x / y, 1)
  expect_equal((x + 5)^x, (xval + 5)^xval)
  expect_equal((x + 5)^y, (xval + 5)^xval)
  expect_equal(as.numeric((x + 5)^x), (xval + 5)^xval)
  expect_equal(as.numeric((x + 5)^y), (xval + 5)^xval)
  expect_equal(x %/% ValueWithUncertainty(xlci, 3, xuci), floor(xval / 3))
})

test_that("ops work properly  - floats", {
  xval <- 10.01
  xlci <- -10.01
  xuci <- 100.01
  x <- ValueWithUncertainty(xlci, xval, xuci)
  y <- ValueWithUncertainty(xlci, xval, xuci)

  expect_equal(as.numeric(x), xval)
  expect_equal(+x, xval)
  expect_equal(x + ValueWithUncertainty(xlci, xval, xuci), 2.0 * xval)
  expect_equal(x + x, 2.0 * xval)
  expect_equal(x + y, 2.0 * xval)
  expect_equal(-x, -xval)
  expect_equal(x - ValueWithUncertainty(xlci, 1.0, xuci), xval - 1.0)
  expect_equal(x - x, 0.0)
  expect_equal(x - y, 0.0)
  expect_equal(ValueWithUncertainty(xlci, 0.0, xuci) * x, 0.0)
  expect_equal(ValueWithUncertainty(xlci, 2.0, xuci) * x, 2.0 * xval)
  expect_equal(x * x, xval * xval)
  expect_equal(x * y, xval * xval)
  expect_equal(x^ValueWithUncertainty(xlci, 2.0, xuci), xval * xval)
  expect_equal(x / ValueWithUncertainty(xlci, 2.0, xuci), xval / 2.0)
  expect_equal(x / x, 1.0)
  expect_equal(x / y, 1.0)
  expect_equal((x + 5.0)^x, (xval + 5.0)^xval)
  expect_equal((x + 5.0)^y, (xval + 5.0)^xval)
  expect_equal(as.numeric((x + 5.0)^x), (xval + 5.0)^xval)
  expect_equal(as.numeric((x + 5.0)^y), (xval + 5.0)^xval)
  expect_equal(x %/% ValueWithUncertainty(xlci, 3.0, xuci), floor(xval / 3.0))
})

test_that("ops work properly - model", {
  xval <- 10.01
  xlci <- -10.01
  xuci <- 100.01
  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample)
  y <- ValueWithUncertainty(xlci, xval, xuci)

  expect_equal(as.numeric(x), xval)
  expect_equal(+x, xval)
  expect_equal(x + ValueWithUncertainty(xlci, xval, xuci), 2.0 * xval)
  expect_equal(x + x, 2.0 * xval)
  expect_equal(x + y, 2.0 * xval)
  expect_equal(-x, -xval)
  expect_equal(x - ValueWithUncertainty(xlci, 1.0, xuci), xval - 1.0)
  expect_equal(x - x, 0.0)
  expect_equal(x - y, 0.0)
  expect_equal(ValueWithUncertainty(xlci, 0.0, xuci) * x, 0.0)
  expect_equal(ValueWithUncertainty(xlci, 2.0, xuci) * x, 2.0 * xval)
  expect_equal(x * x, xval * xval)
  expect_equal(x * y, xval * xval)
  expect_equal(x^ValueWithUncertainty(xlci, 2.0, xuci), xval * xval)
  expect_equal(x / ValueWithUncertainty(xlci, 2.0, xuci), xval / 2.0)
  expect_equal(x / x, 1.0)
  expect_equal(x / y, 1.0)
  expect_equal((x + 5.0)^x, (xval + 5.0)^xval)
  expect_equal((x + 5.0)^y, (xval + 5.0)^xval)
  expect_equal(as.numeric((x + 5.0)^x), (xval + 5.0)^xval)
  expect_equal(as.numeric((x + 5.0)^y), (xval + 5.0)^xval)
  expect_equal(x %/% ValueWithUncertainty(xlci, 3.0, xuci), floor(xval / 3.0))
})
