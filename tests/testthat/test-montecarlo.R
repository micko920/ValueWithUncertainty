
compare_summary_equal <- function(samples, min, qtr1, med, u, qtr3, max, ...) {
  sample_summary <- stats::quantile(samples)
  sample_summary <- signif(c(sample_summary[1L:3L], mean(samples), sample_summary[4L:5L]), 3)
  names(sample_summary) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")

  expect_summary <- c(min, qtr1, med, u, qtr3, max)
  names(expect_summary) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")

  return(expect_equal(sample_summary, expect_summary, ...))
}

rtnRandomSample <- function(v, n, ...) {
  return(round(runif(n, min = min(v), max = max(v))))
}

test_that("basic calc with a value and a arg function", {
  xval <- 10
  xlci <- 1
  xuci <- 100
  x <- ValueWithUncertainty(xlci, xval, xuci)

  calc <- function(v) {
    return(2 * v)
  }


  calcArgs <- function() {
    return(list(x))
  }

  expect_silent(do.call(calc, calcArgs()))
  expect_equal(do.call(calc, calcArgs()), xval * 2)
  expect_equal(x * 2, xval * 2)
})

test_that("basic calc with a runif samples of value with variation", {
  xval <- 10
  xlci <- 0
  xuci <- 20
  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample)

  calc <- function(v) {
    return(2 * v)
  }


  calcArgs <- function() {
    return(list(x))
  }

  ####
  # default bahaviour is to be a fixed value
  expect_silent(do.call(calc, calcArgs()))
  expect_equal(do.call(calc, calcArgs()), xval * 2)
  expect_equal(x * 2, xval * 2)
  stored_result <- x * 2
  expect_equal(stored_result, xval * 2)


  ####
  # Let value of x become a sample of the value in the range based on the model
  set.seed(8121976) # just so tests wont fail for random number changes

  # changing the x here changes the closure of x for the function calcArgs
  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample, fixed = FALSE)
  expect_silent(do.call(calc, calcArgs()))
  expect_equal(do.call(calc, calcArgs()), 26) # using seed

  # now a sample varies with each call
  expect_equal(x * 2, 28) # using seed
  expect_equal(x * 2, 14) # using seed
  expect_equal(x * 2, 24) # using seed
  expect_equal(x * 2, 34) # using seed
  expect_equal(x * 2, 18) # using seed


  # stored result has not changed
  expect_equal(stored_result, xval * 2)

  expect_equal(do.call(calc, calcArgs()), 18) # using seed
  expect_equal(do.call(calc, calcArgs()), 6) # using seed
  expect_equal(do.call(calc, calcArgs()), 6) # using seed
  expect_equal(do.call(calc, calcArgs()), 18) # using seed
  expect_equal(do.call(calc, calcArgs()), 0) # using seed
  expect_equal(do.call(calc, calcArgs()), 20) # using seed
  expect_equal(do.call(calc, calcArgs()), 16) # using seed
  # very simple repeat to simulate value and variation
  samples <- replicate(10000, do.call(calc, calcArgs()))
  expect_equal(samples[1:10], c(28, 24, 14, 0, 8, 36, 28, 34, 18, 0))
  expect_equal(round(mean(samples)), (2 * xval))
  compare_summary_equal(samples, 0, 10, 20, 19.90, 30, 40)
})

test_that("complex example calcs with variation", {
  xval <- 10
  xlci <- 0
  xuci <- 20
  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample)

  calcMult <- function(v) {
    return(2 * v)
  }


  calcMultArgs <- function() {
    return(list(x))
  }

  estA <- calcMult(x)

  expect_equal(estA, 2 * xval)

  yval <- 2
  ylci <- 0
  yuci <- 4
  y <- ValueWithUncertainty(ylci, yval, yuci, model = rtnRandomSample)
  calcShift <- function(v) {
    return(v + 10)
  }


  calcShiftArgs <- function() {
    return(list(y))
  }

  estB <- calcShift(y)

  expect_equal(estB, yval + 10)

  zval <- 100
  zlci <- 50
  zuci <- 150
  z <- ValueWithUncertainty(zlci, zval, zuci, model = rtnRandomSample)
  calcComplex <- function(a, b, c) {
    return((a + b) / c)
  }


  calcComplexArgs <- function() {
    return(list(x, y, z))
  }

  estC <- calcComplex(x, y, z)

  expect_equal(estC, (x + y) / z)

  set.seed(8121976) # just so tests wont fail for random number changes
  ####
  # Let value of x become a sample of the value in the range based on the model
  x <- ValueWithUncertaintySampled(x)
  # very simple repeat to simulate value and variation
  samples <- replicate(10000, do.call(calcMult, calcMultArgs()))
  expect_equal(round(mean(samples)), (2 * xval))
  compare_summary_equal(samples, 0, 10, 20, 19.9, 30, 40)

  y <- ValueWithUncertaintySampled(y)
  # very simple repeat to simulate value and variation
  samples <- replicate(10000, do.call(calcShift, calcShiftArgs()))
  expect_equal(round(mean(samples)), (yval + 10))
  compare_summary_equal(samples, 10, 11, 12, 12, 13, 14)

  z <- ValueWithUncertaintySampled(z)
  # very simple repeat to simulate value and variation
  samples <- replicate(10000, do.call(calcComplex, calcComplexArgs()))
  expect_equal(signif(median(samples), 2), signif(((xval + yval) / zval), 2))
  compare_summary_equal(samples, 0.0000, 0.0708, 0.1210, 0.1330, 0.1780, 0.4600)
})

test_that("chained example calcs with variation", {
  xval <- 10
  xlci <- 0
  xuci <- 20
  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample)

  calcMult <- function(v) {
    return(2 * v)
  }


  calcMultArgs <- function() {
    return(list(x))
  }

  calcMultiModel <- function(v, n, ...) {
    return(replicate(n, do.call(calcMult, calcMultArgs())))
  }

  set.seed(8121976) # just so tests wont fail for random number changes

  # Fixed calculation using x values without variation
  value <- calcMult(x)
  expect_equal(value, 2 * xval)

  ###
  # sampled X values
  x <- ValueWithUncertaintySampled(x)
  samples <- calcMultiModel(x, 10000)
  LCI <- quantile(samples, 0.05)
  expect_equal(as.numeric(LCI), 2)
  UCI <- quantile(samples, 0.95)
  expect_equal(as.numeric(UCI), 38)

  # Chain to next est model
  estA <- ValueWithUncertainty(as.numeric(LCI), value, as.numeric(UCI), model = calcMultiModel)

  # Fixed calculation using estA values without variation
  expect_equal(as.numeric(estA), 2 * xval)
  expect_equal(as.numeric(estA), 2 * xval)
  expect_equal(as.numeric(estA), 2 * xval)
  expect_equal(as.numeric(estA), 2 * xval)


  ###
  # sampled estA values
  estA <- ValueWithUncertaintySampled(estA)
  expect_equal(as.numeric(estA), 4)
  expect_equal(as.numeric(estA), 22)
  expect_equal(as.numeric(estA), 32)
  expect_equal(as.numeric(estA), 8)

  # very simple repeat to simulate value and variation
  samples <- replicate(10000, as.numeric(estA)) # estA is a model of value with variation
  expect_equal(round(mean(samples)), (2 * xval))
  compare_summary_equal(samples, 0, 10, 20, 20.1, 30, 40)

  ###
  # Using a fixed X will mean that estA is fixed as it does not have any other
  # values with variation
  x <- ValueWithUncertaintyFixed(x)
  samples <- replicate(10000, as.numeric(estA)) # estA is a model of value with variation
  expect_equal(round(mean(samples)), (2 * xval))
  compare_summary_equal(samples, 20, 20, 20, 20, 20, 20)

  ###
  # Sampled X will mean that estA is sampled
  # values with variation
  x <- ValueWithUncertaintySampled(x)
  samples <- replicate(10000, as.numeric(estA)) # estA is a model of value with variation
  expect_equal(round(mean(samples)), (2 * xval))
  compare_summary_equal(samples, 0, 10, 20, 20.2, 30, 40)

  ###
  # Sampled X will mean that estA is sampled, unless estA is fixed
  # values with variation
  x <- ValueWithUncertaintySampled(x)
  estA <- ValueWithUncertaintyFixed(estA)
  samples <- replicate(10000, as.numeric(estA)) # estA is a model of value with variation
  expect_equal(round(mean(samples)), (2 * xval))
  compare_summary_equal(samples, 20, 20, 20, 20, 20, 20)
})
