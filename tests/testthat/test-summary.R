



rtnNormSample <- function(v, n, ...) {
  sd <- (diff(range(v)) / (2 * qnorm(p = 0.95))) # LCI,UCI are 95% Q of the sd from mean
  return(rnorm(n, mean = ValueWithUncertaintyValue(v), sd = sd))
}

test_that("summary methods work properly", {
  xval <- 1
  xlci <- -1
  xuci <- 100
  x <- ValueWithUncertainty(xlci, xval, xuci)

  expect_equal(ValueWithUncertaintyMax(x), xuci)
  expect_equal(ValueWithUncertaintyMin(x), xlci)

  expect_equal(max(x), xuci)
  expect_equal(min(x), xlci)

  expect_equal(range(x)[1], xlci)
  expect_equal(range(x)[2], xuci)

  expect_equal(mean(x), xval)

  # expect_equal(weighted.mean(x), xval)

  expect_equal(median(x), 1.253 * xval)

  # expect_equal(quantile(x), quantile(xval))

  xs <- c(xlci,xval,xuci)
  names(xs) <- c("Lower CI", "Value", "Upper CI")
  expect_equal(summary(x),xs)



  xval <- 10
  xsd <- 5
  xlci <- qnorm(p = c(0.05), mean = xval, sd = xsd)
  xuci <- qnorm(p = c(0.95), mean = xval, sd = xsd)
  expect_equal(xsd, ((xuci - xlci) / (2 * qnorm(p = 0.95)))) # LCI,UCI are 95% Q of the sd from mean
  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnNormSample, fixed = TRUE)
  expect_equal(range(x), c(xlci,xuci))
  expect_equal(diff(range(x)), xuci - xlci)
  expect_equal(xsd, (diff(range(x)) / (2 * qnorm(p = 0.95)))) # LCI,UCI are 95% Q of the sd from mean
})

