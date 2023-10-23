library(VGAM)

compare_summary_equal <- function(samples, min, qtr1, med, u, qtr3, max, ...) {
  sample_summary <- stats::quantile(samples)
  sample_summary <- signif(c(sample_summary[1L:3L], mean(samples), sample_summary[4L:5L]), 3)
  names(sample_summary) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")

  expect_summary <- c(min, qtr1, med, u, qtr3, max)
  names(expect_summary) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")

  return(expect_equal(sample_summary, expect_summary, ...))
}

rtnNumeric <- function(v, n, ...) {
  return(rep(as.numeric(v), n))
}

rtnRandomSample <- function(v, n, ...) {
  return(round(runif(n, min = min(v), max = max(v))))
}

rtnNormSample <- function(v, n, ...) {
  sd <- (diff(range(v)) / (2 * qnorm(p = 0.95))) # LCI,UCI are 95% Q of the sd from mean
  return(rnorm(n, mean = ValueWithUncertaintyValue(v), sd = sd))
}

create_rtnSampled <- function(value_samples) {
  return(function(v, n, ...) {
    return(sample(value_samples, size = n, replace = TRUE))
  })
}

test_that("Value ctor", {
  expect_silent(
    ValueWithUncertainty(0, 10, 100)
  )

  expect_error(
    ValueWithUncertainty(0, "a", 100),
    "must be numeric"
  )

  expect_error(
    ValueWithUncertainty("a", 10, 100),
    "must be numeric"
  )

  expect_error(
    ValueWithUncertainty(1, 10, "a"),
    "must be numeric"
  )

  expect_error(
    ValueWithUncertainty(1, 10, 9),
    "UpperCI must be higher than Value"
  )

  expect_error(
    ValueWithUncertainty(12, 10, 19),
    "LowerCI must be lower than Value"
  )

  expect_silent(ValueWithUncertainty(10, 10, 10))
  expect_silent(ValueWithUncertainty(c(10,0), c(10,0), c(10,0)))
})

test_that("Value normal numeric", {
  expect_equal(
    ValueWithUncertainty(0, 1, 10), 1,
    ignore_attr = TRUE
  )
  expect_equal(
    typeof(ValueWithUncertainty(0, 1, 10)), "double",
    ignore_attr = TRUE
  )
})



test_that("Value with model", {
  xval <- 10
  xlci <- 1
  xuci <- 100
  expect_silent(ValueWithUncertainty(xlci, xval, xuci, model = rtnNumeric))

  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnNumeric)
  expect_equal(ValueWithUncertaintyModel(x), rtnNumeric)
  expect_equal(as.numeric(x), xval)
  expect_equal(gen_sample(x, 1, TRUE), c(10))
  expect_equal(gen_sample(x, 10, TRUE), rep(10, 10))
  expect_equal(gen_sample(x, 1, FALSE), c(10))
  expect_equal(gen_sample(x, 10, FALSE), rep(10, 10))


  ####
  # sample generator is a simple return of the value
  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnNumeric)
  expect_equal(as.numeric(x), xval)

  # fixed value is the value
  expect_equal(gen_sample(x, 1, TRUE), c(10))
  expect_equal(gen_sample(x, 10, TRUE), rep(10, 10))

  # sample from range of the value
  expect_equal(gen_sample(x), c(10))

  ####
  # sample generator is a random number from the range
  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample)
  expect_equal(as.numeric(x), 10)

  # fixed value is the value
  expect_equal(gen_sample(x, 1, TRUE), c(10))
  expect_equal(gen_sample(x, 10, TRUE), rep(10, 10))

  # sample from range of the value, based on random seed
  set.seed(8121976) # just so tests wont fail for random number changes
  # sample values for x
  expect_equal(gen_sample(x), c(19))
  expect_equal(
    gen_sample(x, 10),
    c(67, 69, 36, 60, 86, 47, 46, 17, 15, 46)
  )
})

test_that("Value with model and sampled variation", {
  xval <- 10
  xlci <- 1
  xuci <- 100
  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample)
  calc_arg <- function() { return(list(x,x)) }

  ###
  # Fixed by default
  expect_equal(as.numeric(x), xval)
  expect_equal(list(as.numeric(x)), list(xval))
  expect_equal(c(as.numeric(x)), c(xval))
  expect_equal(x + 1, xval + 1) # see test-ops for more testing of ops
  expect_equal(mean(x), 10) # the mean is always the value without variation
  expect_equal(sapply(list(x,x),as.numeric), c(xval,xval))
  expect_equal(sapply(calc_arg(),as.numeric), c(xval,xval))


  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample, fixed = TRUE)
  expect_equal(as.numeric(x), xval)
  expect_equal(list(as.numeric(x)), list(xval))
  expect_equal(c(as.numeric(x)), c(xval))
  expect_equal(x + 1, xval + 1) # see test-ops for more testing of ops
  expect_equal(mean(x), 10) # the mean is always the value without variation
  expect_equal(sapply(list(x,x),as.numeric), c(xval,xval))
  expect_equal(sapply(calc_arg(),as.numeric), c(xval,xval))

  # sample from range of the value, based on random seed
  set.seed(8121976) # just so tests wont fail for random number changes

  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample, fixed = FALSE)
  expect_equal(as.numeric(x), 19)
  expect_equal(list(as.numeric(x)), list(67))
  expect_equal(c(as.numeric(x)), c(69))
  expect_equal(x + 1, 37) # see test-ops for more testing of ops
  expect_equal(mean(x), 10) # the mean is always the value without variation
  expect_equal(sapply(list(x,x),as.numeric), c(60,86))
  expect_equal(sapply(calc_arg(),as.numeric), c(47,46))

  # sample from range of the value, based on random seed
  set.seed(8121976) # just so tests wont fail for random number changes

  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample, fixed = TRUE)
  x <- ValueWithUncertaintySampled(x)
  expect_equal(as.numeric(x), 19)
  expect_equal(list(as.numeric(x)), list(67))
  expect_equal(c(as.numeric(x)), c(69))
  expect_equal(x + 1, 37) # see test-ops for more testing of ops
  expect_equal(mean(x), 10) # the mean is always the value without variation
  expect_equal(sapply(list(x,x),as.numeric), c(60,86))
  expect_equal(sapply(calc_arg(),as.numeric), c(47,46))

  x <- ValueWithUncertaintyFixed(x)
  expect_equal(as.numeric(x), xval)
  expect_equal(list(as.numeric(x)), list(xval))
  expect_equal(c(as.numeric(x)), c(xval))
  expect_equal(x + 1, xval + 1) # see test-ops for more testing of ops
  expect_equal(mean(x), 10) # the mean is always the value without variation
  expect_equal(sapply(list(x,x),as.numeric), c(10,10))
  expect_equal(sapply(calc_arg(),as.numeric), c(10,10))
})


test_that("Values with no variation are fixed", {
  xval <- 10
  xlci <- 1
  xuci <- 20
  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample, fixed = TRUE)
  set.seed(8121976) # just so tests wont fail for random number changes
  expect_equal(as.numeric(x), xval)
  expect_equal(gen_sample(x, 1, TRUE), c(10))
  expect_equal(gen_sample(x, 1, FALSE), c(4))
  expect_equal(gen_sample(x, 10, TRUE), rep(10, 10))
  expect_equal(gen_sample(x, 10, FALSE), c(14, 14, 8, 12, 17, 10, 10, 4, 4, 10))

  # value with no variation
  x <- ValueWithUncertainty(xval, xval, xval, model = rtnRandomSample, fixed = TRUE)
  set.seed(8121976) # just so tests wont fail for random number changes
  expect_equal(as.numeric(x), xval)
  expect_equal(gen_sample(x, 1, TRUE), c(10))
  expect_equal(gen_sample(x, 1, FALSE), c(10))
  expect_equal(gen_sample(x, 10, TRUE), rep(10, 10))
  expect_equal(gen_sample(x, 10, FALSE), rep(10, 10))
})

test_that("Value can be reassigned correctly without variation", {
  xval <- 10
  xlci <- 1
  xuci <- 20
  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnNumeric)
  expect_equal(as.numeric(x), xval)
  expect_equal(gen_sample(x, 1, TRUE), c(10))
  expect_equal(gen_sample(x, 1, FALSE), c(10))
  expect_equal(gen_sample(x, 10, TRUE), rep(10, 10))
  expect_equal(gen_sample(x, 10, FALSE), rep(10, 10))


  x <- ValueWithUncertainty(xlci, x, xuci, model = rtnNumeric)
  expect_equal(as.numeric(x), xval)
  expect_equal(gen_sample(x, 1, TRUE), c(10))
  expect_equal(gen_sample(x, 1, FALSE), c(10))
  expect_equal(gen_sample(x, 10, TRUE), rep(10, 10))
  expect_equal(gen_sample(x, 10, FALSE), rep(10, 10))

  xval <- 10
  xlci <- 1
  xuci <- 20
  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample, fixed = TRUE)
  set.seed(8121976) # just so tests wont fail for random number changes
  expect_equal(as.numeric(x), xval)
  expect_equal(gen_sample(x, 1, TRUE), c(10))
  expect_equal(gen_sample(x, 1, FALSE), c(4))
  expect_equal(gen_sample(x, 10, TRUE), rep(10, 10))
  expect_equal(gen_sample(x, 10, FALSE), c(14, 14, 8, 12, 17, 10, 10, 4, 4, 10))

  x <- ValueWithUncertainty(xlci, x, xuci, model = rtnRandomSample, fixed = TRUE)
  set.seed(8121976) # just so tests wont fail for random number changes
  expect_equal(as.numeric(x), xval)
  expect_equal(gen_sample(x, 1, TRUE), c(10))
  expect_equal(gen_sample(x, 1, FALSE), c(4))
  expect_equal(gen_sample(x, 10, TRUE), rep(10, 10))
  expect_equal(gen_sample(x, 10, FALSE), c(14, 14, 8, 12, 17, 10, 10, 4, 4, 10))

  xval <- 10
  xlci <- 1
  xuci <- 20
  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample, fixed = FALSE)
  set.seed(8121976) # just so tests wont fail for random number changes
  expect_equal(ValueWithUncertaintyValue(x), xval)
  expect_equal(gen_sample(x, 1, TRUE), c(10))
  expect_equal(gen_sample(x, 1, FALSE), c(4))
  expect_equal(gen_sample(x, 10, TRUE), rep(10, 10))
  expect_equal(gen_sample(x, 10, FALSE), c(14, 14, 8, 12, 17, 10, 10, 4, 4, 10))

  x <- ValueWithUncertainty(xlci, x, xuci, model = rtnRandomSample, fixed = FALSE)
  set.seed(8121976) # just so tests wont fail for random number changes
  expect_equal(ValueWithUncertaintyValue(x), xval)
  expect_equal(gen_sample(x, 1, TRUE), c(10))
  expect_equal(gen_sample(x, 1, FALSE), c(4))
  expect_equal(gen_sample(x, 10, TRUE), rep(10, 10))
  expect_equal(gen_sample(x, 10, FALSE), c(14, 14, 8, 12, 17, 10, 10, 4, 4, 10))

  x <- ValueWithUncertainty(xlci, x, xuci, model = rtnRandomSample, fixed = FALSE)
  set.seed(8121976) # just so tests wont fail for random number changes
  expect_equal(ValueWithUncertaintyValue(x), xval)
  expect_equal(gen_sample(x, 1, TRUE), c(10))
  expect_equal(gen_sample(x, 1, FALSE), c(4))
  expect_equal(gen_sample(x, 10, TRUE), rep(10, 10))
  expect_equal(gen_sample(x, 10, FALSE), c(14, 14, 8, 12, 17, 10, 10, 4, 4, 10))
})

test_that("Variable with norm model variation", {
  xval <- 10
  xsd <- 5
  xlci <- qnorm(p = c(0.05), mean = xval, sd = xsd)
  xuci <- qnorm(p = c(0.95), mean = xval, sd = xsd)
  set.seed(8121976) # just so tests wont fail for random number changes
  example_samples <- rnorm(1.0e+06, mean = xval, sd = xsd)
  expect_equal(round(mean(example_samples)), xval)
  expect_equal(signif(sd(example_samples), 3), xsd)
  compare_summary_equal(example_samples, -14.1, 6.62, 10, 10, 13.4, 37.3)

  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnNormSample, fixed = TRUE)

  expect_equal(xsd, (diff(range(x)) / (2 * qnorm(p = 0.95)))) # LCI,UCI are 95% Q of the sd from mean
  set.seed(8121976) # just so tests wont fail for random number changes
  samples <- rnorm(n = 10, mean = ValueWithUncertaintyValue(x), sd = xsd)
  expect_equal(
    signif(samples, 7),
    c(
      5.413773, 12.391030, 11.198530, 9.493530, 5.138721, 9.442607, 9.736205,
      12.671910, 7.940212, 5.772389
    )
  )
  set.seed(8121976) # just so tests wont fail for random number changes
  samples <- replicate(10, rtnNormSample(x, 1))
  expect_equal(
    signif(samples, 7),
    c(
      5.413773, 12.391030, 11.198530, 9.493530, 5.138721, 9.442607, 9.736205,
      12.671910, 7.940212, 5.772389
    )
  )

  set.seed(8121976) # just so tests wont fail for random number changes
  expect_equal(as.numeric(x), xval)
  expect_equal(gen_sample(x, 1, TRUE), c(10))
  expect_equal(signif(gen_sample(x, 1, FALSE), 7), c(5.413773))
  expect_equal(gen_sample(x, 10, TRUE), rep(10, 10))


  set.seed(8121976) # just so tests wont fail for random number changes
  samples <- gen_sample(x, 1.0e+06, FALSE)
  expect_equal(
    signif(samples[1:10], 7),
    c(
      5.413773, 12.391030, 11.198530, 9.493530, 5.138721, 9.442607, 9.736205,
      12.671910, 7.940212, 5.772389
    )
  )
  expect_equal(round(mean(samples)), xval)
  expect_equal(signif(sd(samples), 3), xsd)
  expect_equal(summary(samples), summary(example_samples))
  compare_summary_equal(samples, -14.1, 6.62, 10, 10, 13.4, 37.3)
})



test_that("Variable with published norm model", {
  xval <- 10
  xsd <- 5
  xlci <- qnorm(p = c(0.05), mean = xval, sd = xsd)
  xuci <- qnorm(p = c(0.95), mean = xval, sd = xsd)
  set.seed(8121976) # just so tests wont fail for random number changes
  example_samples <- rnorm(1.0e+06, mean = xval, sd = xsd)
  expect_equal(round(mean(example_samples)), xval)
  expect_equal(signif(sd(example_samples), 3), xsd)
  compare_summary_equal(example_samples, -14.1, 6.62, 10, 10, 13.4, 37.3)

  x <- ValueWithUncertainty(xlci, xval, xuci, model = vwuNorm, fixed = TRUE)

  expect_equal(xsd, (diff(range(x)) / (2 * qnorm(p = 0.95)))) # LCI,UCI are 95% Q of the sd from mean
  set.seed(8121976) # just so tests wont fail for random number changes
  samples <- rnorm(n = 10, mean = ValueWithUncertaintyValue(x), sd = xsd)
  expect_equal(
    signif(samples, 7),
    c(
      5.413773, 12.391030, 11.198530, 9.493530, 5.138721, 9.442607, 9.736205,
      12.671910, 7.940212, 5.772389
    )
  )
  set.seed(8121976) # just so tests wont fail for random number changes
  samples <- replicate(10, vwuNorm(x, 1))
  expect_equal(
    signif(samples, 7),
    c(
      5.413773, 12.391030, 11.198530, 9.493530, 5.138721, 9.442607, 9.736205,
      12.671910, 7.940212, 5.772389
    )
  )

  set.seed(8121976) # just so tests wont fail for random number changes
  expect_equal(as.numeric(x), xval)
  expect_equal(gen_sample(x, 1, TRUE), c(10))
  expect_equal(signif(gen_sample(x, 1, FALSE), 7), c(5.413773))
  expect_equal(gen_sample(x, 10, TRUE), rep(10, 10))


  set.seed(8121976) # just so tests wont fail for random number changes
  samples <- gen_sample(x, 1.0e+06, FALSE)
  expect_equal(
    signif(samples[1:10], 7),
    c(
      5.413773, 12.391030, 11.198530, 9.493530, 5.138721, 9.442607, 9.736205,
      12.671910, 7.940212, 5.772389
    )
  )
  expect_equal(round(mean(samples)), xval)
  expect_equal(signif(sd(samples), 3), xsd)
  expect_equal(summary(samples), summary(example_samples))
  compare_summary_equal(samples, -14.1, 6.62, 10, 10, 13.4, 37.3)
})

test_that("Variable with published triangle model", {
  xval <- 10
  xlci <- 5
  xuci <- 15
  set.seed(8121976) # just so tests wont fail for random number changes
  example_samples <- rtriangle(1.0e+06, theta = xval, lower = xlci, upper = xuci)
  expect_equal(round(mean(example_samples)), xval)
  compare_summary_equal(example_samples, 5.0, 8.54, 10, 10, 11.5, 15)


  x <- ValueWithUncertainty(xlci, xval, xuci, model = vwuTriangle, fixed = TRUE)
  expect_equal(xval, ValueWithUncertaintyValue(x))
  expect_equal(c(xlci, xuci), range(x))
  set.seed(8121976) # just so tests wont fail for random number changes
  samples <- replicate(10, rtriangle(n = 1, theta = ValueWithUncertaintyValue(x), lower = xlci, upper = xuci))
  expect_equal(
    signif(samples, 7),
    c(
      10.90746, 11.14409,  8.389906,  6.889770, 11.9365,
      8.838043, 10.30816,  9.640842, 12.29018, 13.58133
    )
  )

  set.seed(8121976) # just so tests wont fail for random number changes
  samples <- replicate(10, vwuTriangle(x, 1))
  expect_equal(
    signif(samples, 7),
    c(
      10.90746, 11.14409,  8.389906,  6.889770, 11.9365,
      8.838043, 10.30816,  9.640842, 12.29018, 13.58133
    )
  )

  set.seed(8121976) # just so tests wont fail for random number changes
  expect_equal(as.numeric(x), xval)
  expect_equal(gen_sample(x, 1, TRUE), c(10))
  expect_equal(signif(gen_sample(x, 1, FALSE), 7), c(10.90746))
  expect_equal(gen_sample(x, 10, TRUE), rep(10, 10))


  set.seed(8121976) # just so tests wont fail for random number changes
  samples <- gen_sample(x, 1.0e+06, FALSE)
  expect_equal(
    signif(samples[1:10], 7),
    c(
      12.466610, 11.0665, 13.341190,  7.987118, 13.213990,
      12.209770,  8.389906,  8.381778, 11.163600,  6.889770
    )
  )
  expect_equal(round(mean(samples)), xval)
  expect_equal(c(xlci, xuci), range(xlci, xuci))
  expect_equal(summary(samples), summary(example_samples))
  compare_summary_equal(samples, 5.0, 8.54, 10, 10, 11.5, 15)
})

test_that("Variable with sampled model variation", {
  xval <- 10
  xsd <- 5
  xlci <- qnorm(p = c(0.05), mean = xval, sd = xsd)
  xuci <- qnorm(p = c(0.95), mean = xval, sd = xsd)
  set.seed(8121976) # just so tests wont fail for random number changes
  example_samples <- rnorm(1.0e+06, mean = xval, sd = xsd)
  expect_equal(round(mean(example_samples)), xval)
  expect_equal(signif(sd(example_samples), 3), xsd)
  compare_summary_equal(example_samples, -14.1, 6.62, 10, 10, 13.4, 37.3)

  x <- ValueWithUncertainty(xlci, xval, xuci, model = create_rtnSampled(example_samples), fixed = TRUE)

  set.seed(8121976) # just so tests wont fail for random number changes
  samples <- gen_sample(x, 2.0e+06, FALSE)
  expect_equal(round(mean(samples)), xval)
  expect_equal(signif(sd(samples), 3), xsd)
  # close but allow for differance due to low sample of samples issue
  expect_equal(signif(summary(samples), 3), signif(summary(example_samples), 3))
  compare_summary_equal(samples, -14.1, 6.62, 10, 10, 13.4, 37.3)
})


test_that("Variable with sampled model variation", {
  xval <- 10
  xsd <- 5
  xlci <- qnorm(p = c(0.05), mean = xval, sd = xsd)
  xuci <- qnorm(p = c(0.95), mean = xval, sd = xsd)
  set.seed(8121976) # just so tests wont fail for random number changes
  example_samples <- rnorm(1.0e+06, mean = xval, sd = xsd)
  expect_equal(round(mean(example_samples)), xval)
  expect_equal(signif(sd(example_samples), 3), xsd)
  compare_summary_equal(example_samples, -14.1, 6.62, 10, 10, 13.4, 37.3)

  x <- ValueWithUncertainty(xlci, xval, xuci, model = create_vwuSampled(example_samples), fixed = TRUE)

  set.seed(8121976) # just so tests wont fail for random number changes
  samples <- gen_sample(x, 2.0e+06, FALSE)
  expect_equal(round(mean(samples)), xval)
  expect_equal(signif(sd(samples), 3), xsd)
  # close but allow for differance due to low sample of samples issue
  expect_equal(signif(summary(samples), 3), signif(summary(example_samples), 3))
  compare_summary_equal(samples, -14.1, 6.62, 10, 10, 13.4, 37.3)
})


test_that("Value works with name", {
  xval <- 10
  xlci <- 1
  xuci <- 100
  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample)
  expect_silent(names(x) <- c("myValue"))
  expect_equal(names(x), c("myValue"))

  ###
  # Fixed by default
  expect_equal(as.numeric(x), xval)
  expect_equal(list(as.numeric(x)), list(xval))
  expect_equal(c(as.numeric(x)), c(xval))
  expect_equal(x + 1, xval + 1) # see test-ops for more testing of ops
  expect_equal(mean(x), 10) # the mean is always the value without variation

  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample, fixed = TRUE)
  expect_silent(names(x) <- c("myValue"))
  expect_equal(names(x), c("myValue"))
  expect_equal(as.numeric(x), xval)
  expect_equal(list(as.numeric(x)), list(xval))
  expect_equal(c(as.numeric(x)), c(xval))
  expect_equal(x + 1, xval + 1) # see test-ops for more testing of ops
  expect_equal(mean(x), 10) # the mean is always the value without variation

  # sample from range of the value, based on random seed
  set.seed(8121976) # just so tests wont fail for random number changes

  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample, fixed = FALSE)
  expect_silent(names(x) <- c("myValue"))
  expect_equal(names(x), c("myValue"))
  expect_equal(as.numeric(x), 19)
  expect_equal(list(as.numeric(x)), list(67))
  expect_equal(c(as.numeric(x)), c(69))
  expect_equal(x + 1, 37) # see test-ops for more testing of ops
  expect_equal(mean(x), 10) # the mean is always the value without variation

  # sample from range of the value, based on random seed
  set.seed(8121976) # just so tests wont fail for random number changes

  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample, fixed = TRUE)
  expect_silent(names(x) <- c("myValue"))
  expect_equal(names(x), c("myValue"))
  x <- ValueWithUncertaintySampled(x)
  expect_equal(names(x), c("myValue"))
  expect_equal(as.numeric(x), 19)
  expect_equal(list(as.numeric(x)), list(67))
  expect_equal(c(as.numeric(x)), c(69))
  expect_equal(x + 1, 37) # see test-ops for more testing of ops
  expect_equal(mean(x), 10) # the mean is always the value without variation


  x <- ValueWithUncertaintyFixed(x)
  expect_equal(names(x), c("myValue"))
  expect_equal(as.numeric(x), xval)
  expect_equal(list(as.numeric(x)), list(xval))
  expect_equal(c(as.numeric(x)), c(xval))
  expect_equal(x + 1, xval + 1) # see test-ops for more testing of ops
  expect_equal(mean(x), 10) # the mean is always the value without variation
})



test_that("Value works with value without uncertainty", {
  xval <- 10
  xlci <- 10
  xuci <- 10
  f <- function(v, n, ...) {
    stop("Dont call the model", call. = FALSE)
    return(NA)
  }
  expect_silent(x <- ValueWithUncertainty(xlci, xval, xuci, model = f, fixed = FALSE))
  expect_silent(as.numeric(x))

  expect_error(x <- ValueWithUncertainty(11, xval, 1, model = f, fixed = FALSE))
  expect_error(x <- ValueWithUncertainty(c(0,11), c(0,xval), c(0,1), model = f, fixed = FALSE))




  expect_silent(x <- ValueWithUncertainty(0,0,0, model = f, fixed = FALSE))
  expect_silent(as.numeric(x))

  expect_silent(x <- ValueWithUncertainty(c(1,0),c(2,0),c(3,0), model = f, fixed = FALSE))

  expect_error(as.numeric(x[[1]]))
  expect_silent(as.numeric(x[[2]]))

})



test_that("Value works with array params", {

  xval <- 10
  xlci <- 1
  xuci <- 100

  calc_arg <- function() { return(x) }


  x <- ValueWithUncertainty(c(xlci,xlci), c(xval,xval), c(xuci,xuci), model = rtnRandomSample, fixed = TRUE)
  expect_equal(as.numeric(x), c(xval,xval))
  expect_equal(list(as.numeric(x)), list(c(xval,xval)))
  expect_equal(c(as.numeric(x)), c(xval,xval))
  expect_equal(sapply(x,"+", 1), c(xval + 1, xval +1)) # see test-ops for more testing of ops
  expect_equal(sapply(x, mean), c(10,10)) # the mean is always the value without variation
  expect_equal(sapply(x,as.numeric), c(xval,xval))
  expect_equal(sapply(calc_arg(),as.numeric), c(xval,xval))

  # sample from range of the value, based on random seed
  set.seed(8121976) # just so tests wont fail for random number changes

  x <- ValueWithUncertainty(c(xlci,xlci), c(xval,xval), c(xuci,xuci), model = rtnRandomSample, fixed = FALSE)
  expect_equal(sapply(x, as.numeric), c(19,67))
  expect_equal(sapply(x,"+", 1), c(70,37)) # see test-ops for more testing of ops
  expect_equal(sapply(x, function(v) { mean(as.numeric(v))}), c(60,86)) # the mean is always the value without variation
  expect_equal(sapply(x,as.numeric), c(47,46))
  expect_equal(sapply(calc_arg(),as.numeric), c(17,15))


  x <- ValueWithUncertaintyFixed(x)

  expect_equal(sapply(x,class),c("ValueWithUncertainty", "ValueWithUncertainty"))

  expect_equal(as.numeric(x), c(xval,xval))
  expect_equal(list(as.numeric(x)), list(c(xval,xval)))
  expect_equal(c(as.numeric(x)), c(xval,xval))
  expect_equal(sapply(x,"+", 1), c(xval + 1, xval +1)) # see test-ops for more testing of ops
  expect_equal(sapply(x, mean), c(10,10)) # the mean is always the value without variation
  expect_equal(sapply(x,as.numeric), c(xval,xval))
  expect_equal(sapply(calc_arg(),as.numeric), c(xval,xval))

  expect_silent(x <- ValueWithUncertaintySampled(x))
  expect_equal(sapply(x,class),c("ValueWithUncertainty", "ValueWithUncertainty"))
  expect_silent(x <- ValueWithUncertaintyValue(x))
  expect_equal(sapply(x,class),c("numeric", "numeric"))


  expect_error(x <- ValueWithUncertainty(c(0,11), c(0,xval), c(0,1), model = f, fixed = FALSE))

  expect_silent(x <- ValueWithUncertainty(c(1,0),c(2,0),c(3,0), fixed = FALSE))

  expect_silent(as.numeric(x[[1]]))
  expect_silent(as.numeric(x[[2]]))

})

