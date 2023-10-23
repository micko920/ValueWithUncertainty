# Digits affects testing. Wow! This is a side affect of using the printed
# output for comparisone rather than the value.
# So when doing floating point stuff we need to be
# sure we can see the values.
options(digits=22)

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


# This is too test that a floating point number value with very small upper and
# lower CI can be clamped to the fixed value.
# This is to help with the mean of a repeating sequence with very small
# variation causing the value to skew or the triangle function to complain.

createFunc_equalWithTol <- function(tol_value = 1e-07, margin = 9e-08) {
  return(function(l, e, u) {
    nl <- l
    nu <- u
    if (abs(e) < 1e+04) {
      scale <- 1.0e+04
	  t <- tol_value * 1.0^log10(abs(e)) - 4
    } else {
      scale <- 1.0
	  t <- tol_value
    }
    if (isTRUE(all.equal(l * scale, e * scale, tolerance = t))) {
      nl <- e - (margin / scale)
    }
    if (isTRUE(all.equal(e * scale, u * scale, tolerance = t))) {
      nu <- e + (margin / scale)
    }
    if (isTRUE(all.equal(l * scale, e * scale, tolerance = t)) &&
      isTRUE(all.equal(e * scale, u * scale, tolerance = t))) {
      return(c(e, e, e))
    } else {
      return(c(nl, e, nu))
    }
  })
}


createFunc_rtnRandomSampleWithTolerance <- function(equalFunc) {
  return(function(v, n, ...) {
    est <- ValueWithUncertaintyValue(v)
    lci <- min(v)
    uci <- max(v)
    rng <- do.call(equalFunc, list(lci, est, uci))
    if (isTRUE(all.equal(rng[1], rng[2], rng[3], tolerance = 1e-18))) {
      return(est)
    } else {
      return(runif(n = n, min = rng[1], max = rng[3]))
    }
  })
}



createEstValueModel <- function(value, calc, calcArgs) {
  calcModel <- function(v, n, ...) {
    return(replicate(n, do.call(calc, calcArgs())))
  }
  ###
  # sampled X values
  samples <- replicate(10000, do.call(calc, calcArgs()))
  LCI <- quantile(samples, 0.05)
  UCI <- quantile(samples, 0.95)

  # fixed is default, but for this testing we need to have variation for
  # chaining calculations
  return(ValueWithUncertainty(as.numeric(LCI), value, as.numeric(UCI),
    model = calcModel, fixed = FALSE
  ))
}


test_that("complex example chained calcs with variation", {

  set.seed(8121976) # just so tests wont fail for random number changes
  calc_A <- function(v) {
    return(2 * v)
  }


  calc_A_Args <- function() {
    return(list(x))
  }

  calc_B <- function(v) {
    return(v + 10)
  }


  calc_B_Args <- function() {
    return(list(y))
  }

  calc_C <- function(a, b, c) {
    return((a + b) / c)
  }


  calc_C_Args <- function() {
    return(list(estA, estB, z))
  }
  xval <- 10
  xlci <- 0
  xuci <- 20
  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample, fixed=FALSE)

  yval <- 2
  ylci <- 0
  yuci <- 4
  y <- ValueWithUncertainty(ylci, yval, yuci, model = rtnRandomSample, fixed=FALSE)

  zval <- 100
  zlci <- 50
  zuci <- 150
  z <- ValueWithUncertainty(zlci, zval, zuci, model = rtnRandomSample, fixed=FALSE)

  # Chain to next est model
  estA <- ValueWithUncertaintyFixed(createEstValueModel(calc_A(x), calc_A, calc_A_Args))
  estB <- ValueWithUncertaintyFixed(createEstValueModel(calc_B(y), calc_B, calc_B_Args))

  x <- ValueWithUncertaintyFixed(x)
  y <- ValueWithUncertaintyFixed(y)
  z <- ValueWithUncertaintyFixed(z)
  estA <- ValueWithUncertaintySampled(estA)
  estB <- ValueWithUncertaintySampled(estB)

  estC <- calc_C(x, y, z)

  # No variation
  expect_equal(estC, (x + y) / z)

  ###
  # No variation
  # very simple repeat to simulate value and variation
  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  compare_summary_equal(samples, 0.32, 0.32, 0.32, 0.32, 0.32, 0.32)

  ###
  # Let Z be sampled
  set.seed(8121976) # just so tests wont fail for random number changes
  x <- ValueWithUncertaintyFixed(x)
  y <- ValueWithUncertaintyFixed(y)
  z <- ValueWithUncertaintySampled(z)
  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  compare_summary_equal(samples, 0.213, 0.256, 0.32, 0.352, 0.427, 0.64)

  ###
  # Let Y be sampled
  set.seed(8121976) # just so tests wont fail for random number changes
  x <- ValueWithUncertaintyFixed(x)
  y <- ValueWithUncertaintySampled(y)
  z <- ValueWithUncertaintyFixed(z)


  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  compare_summary_equal(samples, 0.3000, 0.3100, 0.3200, 0.3200, 0.3300, 0.3400)

  ###
  # Let X be sampled
  set.seed(8121976) # just so tests wont fail for random number changes
  x <- ValueWithUncertaintySampled(x)
  y <- ValueWithUncertaintyFixed(y)
  z <- ValueWithUncertaintyFixed(z)
  estA <- createEstValueModel(calc_A(x), calc_A, calc_A_Args)
  estB <- createEstValueModel(calc_B(y), calc_B, calc_B_Args)
  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  compare_summary_equal(samples, 0.1200, 0.2200, 0.3200, 0.3210, 0.4200, 0.5200)
})

test_that("sensitivity analysis of complex example chained calcs with variation ", {
  calc_A <- function(v) {
    return(2 * v)
  }
  calc_A_Args <- function() {
    return(list(x))
  }
  calc_B <- function(v) {
    return(v + 10)
  }
  calc_B_Args <- function() {
    return(list(y))
  }
  calc_C <- function(a, b, c) {
    return((a + b) / c)
  }
  calc_C_Args <- function() {
    return(list(estA, estB, z))
  }


  # Medium variance used in multiplication  and numerators
  xval <- 10
  xlci <- 0
  xuci <- 20
  x <- ValueWithUncertainty(xlci, xval, xuci, model = rtnRandomSample)

  # Low variance with limited impact to calculations (small value used in
  # addition in numerators)
  yval <- 2
  ylci <- 0
  yuci <- 4
  y <- ValueWithUncertainty(ylci, yval, yuci, model = rtnRandomSample)


  # Larger variance used in denominator, no multiplication
  zval <- 100
  zlci <- 70
  zuci <- 130
  z <- ValueWithUncertainty(zlci, zval, zuci, model = rtnRandomSample)

  # Chain to next est model
  estA <- createEstValueModel(calc_A(x), calc_A, calc_A_Args)
  estB <- createEstValueModel(calc_B(y), calc_B, calc_B_Args)
  estC <- createEstValueModel(calc_C(estA, estB, z), calc_C, calc_C_Args)
  expect_equal(calc_C(estA, estB, z), ((2.0 * xval) + (yval + 10.0)) / zval)



  x <- ValueWithUncertaintySampled(x)
  y <- ValueWithUncertaintySampled(y)
  z <- ValueWithUncertaintySampled(z)

  set.seed(8121976) # just so tests wont fail for random number changes

  # Results can be calculated without Monte Carlo runs
  expect_equal(mean(estA), 2.0 * mean(x))
  expect_equal(mean(estB), mean(y) + 10.0)
  expect_equal(mean(estC), ((2.0 * xval) + (yval + 10.0)) / zval)

  set.seed(8121976) # just so tests wont fail for random number changes
  estA <- createEstValueModel(calc_A(x), calc_A, calc_A_Args)
  estB <- createEstValueModel(calc_B(y), calc_B, calc_B_Args)
  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  V_all <- var(samples)


  ###
  # Total Effect Index of value (x) is  1 minus the variance effect of all
  # other variables.

  # Total Effect Index of X, i.e. fixed X
  set.seed(8121976) # just so tests wont fail for random number changes
  x <- ValueWithUncertaintyFixed(x)
  y <- ValueWithUncertaintySampled(y)
  z <- ValueWithUncertaintySampled(z)
  estA <- createEstValueModel(calc_A(x), calc_A, calc_A_Args)
  estB <- createEstValueModel(calc_B(y), calc_B, calc_B_Args)
  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  TEI_X <- 1 - (var(samples) / V_all)
  expect_equal(signif(TEI_X, 4), 0.7984)

  # Total Effect Index of Y, i.e. fixed Y
  set.seed(8121976) # just so tests wont fail for random number changes
  x <- ValueWithUncertaintySampled(x)
  y <- ValueWithUncertaintyFixed(y)
  z <- ValueWithUncertaintySampled(z)
  estA <- createEstValueModel(calc_A(x), calc_A, calc_A_Args)
  estB <- createEstValueModel(calc_B(y), calc_B, calc_B_Args)
  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  TEI_Y <- 1 - (var(samples) / V_all)
  expect_equal(signif(TEI_Y, 4), 0.006862)

  # Total Effect Index of Z, i.e. fixed Z
  set.seed(8121976) # just so tests wont fail for random number changes
  x <- ValueWithUncertaintySampled(x)
  y <- ValueWithUncertaintySampled(y)
  z <- ValueWithUncertaintyFixed(z)
  estA <- createEstValueModel(calc_A(x), calc_A, calc_A_Args)
  estB <- createEstValueModel(calc_B(y), calc_B, calc_B_Args)
  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  TEI_Z <- 1 - (var(samples) / V_all)
  expect_equal(signif(TEI_Z, 4), 0.2674)
})

# test_that("tolerance clamp", {
#   equalFunc <- createFunc_equalWithTol(tol_value = 1e-8, margin = 9e-9)
#   expect_equal(equalFunc(10, 10, 10), c(10, 10, 10), tolerance = 1e-19)
#   expect_equal(equalFunc(10, 10, 20), c(10 - 9e-13, 10, 20), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 10, 10), c(0, 10, 10 + 9e-13), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 10, 20), c(0, 10, 20), tolerance = 1e-19)
#   expect_equal(equalFunc(1, 1, 1), c(1, 1, 1), tolerance = 1e-19)
#   expect_equal(equalFunc(1, 1, 10), c(1 - 9e-13, 1, 10), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 1, 1), c(0, 1, 1 + 9e-13), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 1, 10), c(0, 1, 10), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 1, 1 + 1e-07), c(0, 1, 1 + 1e-07), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 1, 1 + 1e-10), c(0, 1, 1 + 9e-13), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 1, 1 + 1e-11), c(0, 1, 1 + 9e-13), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 1, 1 + 1e-12), c(0, 1, 1 + 9e-13), tolerance = 1e-19)
#   expect_equal(equalFunc(1 - 1e-07, 1, 1 + 1e-07), c(1 - 1e-07, 1, 1 + 1e-07), tolerance = 1e-19)
#   expect_equal(equalFunc(1 - 1e-13, 1, 1 + 1e-13), c(1, 1, 1), tolerance = 1e-19)

#   equalFunc <- createFunc_equalWithTol(tol_value = 1e-4, margin = 9e-5)
#   expect_equal(equalFunc(10, 10, 10), c(10, 10, 10), tolerance = 1e-19)
#   expect_equal(equalFunc(10, 10, 20), c(10 - 9e-09, 10, 20), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 10, 10), c(0, 10, 10 + 9e-09), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 10, 20), c(0, 10, 20), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 1, 1 + 1e-3), c(0, 1, 1 + 1e-3), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 1, 1 + 1e-4), c(0, 1, 1 + 9e-09), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 1, 1 + 1e-6), c(0, 1, 1 + 9e-09), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 1, 1 + 1e-7), c(0, 1, 1 + 9e-09), tolerance = 1e-19)
#   expect_equal(equalFunc(1 - 1e-7, 1, 1 + 1e-7), c(1, 1, 1), tolerance = 1e-19)
#   expect_equal(equalFunc(1 - 1e-3, 1, 1 + 1e-3), c(1 - 1e-3, 1, 1 + 1e-3), tolerance = 1e-19)

#   equalFunc <- createFunc_equalWithTol(tol_value = 1e-6, margin = 9e-07)
#   expect_equal(equalFunc(0.0001, 0.0001, 0.0001), c(0.0001, 0.0001, 0.0001), tolerance = 1e-29)
#   expect_equal(equalFunc(0.0001, 0.0001, 0.0002), c(0.0001 - 9e-11, 0.0001, 0.0002), tolerance = 1e-29)
#   expect_equal(equalFunc(0, 0.0001, 0.0001), c(0, 0.0001, 0.0001 + 9e-11), tolerance = 1e-29)
#   expect_equal(equalFunc(0, 0.0001, 0.0002), c(0, 0.0001, 0.0002), tolerance = 1e-29)
#   expect_equal(equalFunc(0, 0.0001, 0.0001 + 1e-3), c(0, 0.0001, 0.0001 + 1e-3), tolerance = 1e-29)
#   expect_equal(equalFunc(0, 0.0001, 0.0001 + 1e-10), c(0, 0.0001, 0.0001 + 9e-11), tolerance = 1e-29)
#   expect_equal(equalFunc(0, 0.0001, 0.0001 + 1e-11), c(0, 0.0001, 0.0001 + 9e-11), tolerance = 1e-29)
#   expect_equal(equalFunc(0, 0.0001, 0.0001 + 1e-12), c(0, 0.0001, 0.0001 + 9e-11), tolerance = 1e-29)
#   expect_equal(equalFunc(0.0001 - 1e-11, 0.0001, 0.0001 + 1e-11), c(0.0001, 0.0001, 0.0001), tolerance = 1e-29)
#   expect_equal(equalFunc(0.0001 - 1e-3, 0.0001, 0.0001 + 1e-3), c(0.0001 - 1e-3, 0.0001, 0.0001 + 1e-3), tolerance = 1e-29)

#   equalFunc <- createFunc_equalWithTol(tol_value = 1e-1, margin = 9e-2)
#   expect_equal(equalFunc(1e+05, 1e+05, 1e+05), c(1e+05, 1e+05, 1e+05), tolerance = 1e-29)
#   expect_equal(equalFunc(1e+05, 1e+05, 2e+05), c(1e+05 - 9e-02, 1e+05, 2e+05), tolerance = 1e-29)
#   expect_equal(equalFunc(10, 10, 10), c(10, 10, 10), tolerance = 1e-29)
#   expect_equal(equalFunc(10, 10, 20), c(10 - 9e-06, 10, 20), tolerance = 1e-29)
#   expect_equal(equalFunc(0.0001, 0.0001, 0.0001), c(0.0001, 0.0001, 0.0001), tolerance = 1e-29)
#   expect_equal(equalFunc(0.0001, 0.0001, 0.0002), c(0.0001 - 9e-6, 0.0001, 0.0002), tolerance = 1e-29)

#   expect_equal(equalFunc(0, 1e+05, 1e+05), c(0, 1e+05, 1e+05 + 9e-2), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 1e+05, 2e+05), c(0, 1e+05, 2e+05), tolerance = 1e-19)
#   expect_equal(equalFunc(10, 10, 10), c(10, 10, 10), tolerance = 1e-29)
#   expect_equal(equalFunc(10, 10, 20), c(10 - 9e-06, 10, 20), tolerance = 1e-29)
#   expect_equal(equalFunc(0, 0.0001, 0.0001), c(0, 0.0001, 0.0001 + 9e-6), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 0.0001, 0.0002), c(0, 0.0001, 0.0002), tolerance = 1e-19)

#   equalFunc <- createFunc_equalWithTol(tol_value = 5e-1, margin = 9e-2)
#   expect_equal(equalFunc(1e+05, 1e+05, 1e+05), c(1e+05, 1e+05, 1e+05), tolerance = 1e-29)
#   expect_equal(equalFunc(1e+05, 1e+05, 2e+05), c(1e+05 - 9e-2, 1e+05, 2e+05), tolerance = 1e-29)
#   expect_equal(equalFunc(10, 10, 10), c(10, 10, 10), tolerance = 1e-29)
#   expect_equal(equalFunc(10, 10, 20), c(10 - 9e-06, 10, 20), tolerance = 1e-29)
#   expect_equal(equalFunc(0.0001, 0.0001, 0.0001), c(0.0001, 0.0001, 0.0001), tolerance = 1e-29)
#   expect_equal(equalFunc(0.0001, 0.0001, 0.0002), c(0.0001 - 9e-06, 0.0001, 0.0002), tolerance = 1e-29)

#   expect_equal(equalFunc(0, 1e+05, 1e+05), c(0, 1e+05, 1e+05 + 9e-2), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 1e+05, 2e+05), c(0, 1e+05, 2e+05), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 10, 10), c(0, 10, 10 + 9e-06), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 10, 20), c(0, 10, 20), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 0.0001, 0.0001), c(0, 0.0001, 0.0001 + 9e-06), tolerance = 1e-19)
#   expect_equal(equalFunc(0, 0.0001, 0.0002), c(0, 0.0001, 0.0002), tolerance = 1e-19)

#   expect_equal(equalFunc(87345.1234, 87345, 87345.322), c(87345, 87345, 87345), tolerance = 1e-19)
#   expect_equal(equalFunc(0.47 - (0.47 * 0.5), 0.47, 0.47 + (0.47 * 0.5)), c(0.235, 0.47, 0.705), tolerance = 1e-19)
# })

test_that("sensitivity analysis of calcs with small variation and tolerance clamp", {
  calc_A <- function(v) {
    return(2 * v)
  }
  calc_A_Args <- function() {
    return(list(x))
  }
  calc_B <- function(v) {
    return(v + 10)
  }
  calc_B_Args <- function() {
    return(list(y))
  }
  calc_C <- function(a, b, c) {
    return((a + b) / c)
  }
  calc_C_Args <- function() {
    return(list(estA, estB, z))
  }

  equalFunc <- createFunc_equalWithTol(tol_value = 1e-24, margin = 9e-25)
  sampleModel <- createFunc_rtnRandomSampleWithTolerance(equalFunc)

  # Medium variance used in multiplication  and numerators
  xval <- 10
  xlci <- 0
  xuci <- 20
  x <- ValueWithUncertainty(xlci, xval, xuci, model = sampleModel)

  # Low variance with limited impact to calculations (small value used in
  # addition in numerators)
  yval <- 2
  ylci <- 0
  yuci <- 4
  y <- ValueWithUncertainty(ylci, yval, yuci, model = sampleModel)


  # Larger variance used in denominator, no multiplication
  zval <- 100
  zlci <- 70
  zuci <- 130
  z <- ValueWithUncertainty(zlci, zval, zuci, model = sampleModel)

  # Chain to next est model
  estA <- createEstValueModel(calc_A(x), calc_A, calc_A_Args)
  estB <- createEstValueModel(calc_B(y), calc_B, calc_B_Args)
  estC <- createEstValueModel(calc_C(estA, estB, z), calc_C, calc_C_Args)
  expect_equal(calc_C(estA, estB, z), ((2.0 * xval) + (yval + 10.0)) / zval)



  x <- ValueWithUncertaintySampled(x)
  y <- ValueWithUncertaintySampled(y)
  z <- ValueWithUncertaintySampled(z)

  set.seed(8121976) # just so tests wont fail for random number changes

  # Results can be calculated without Monte Carlo runs
  expect_equal(mean(estA), 2.0 * mean(x))
  expect_equal(mean(estB), mean(y) + 10.0)
  expect_equal(mean(estC), ((2.0 * xval) + (yval + 10.0)) / zval)

  set.seed(8121976) # just so tests wont fail for random number changes
  estA <- createEstValueModel(calc_A(x), calc_A, calc_A_Args)
  estB <- createEstValueModel(calc_B(y), calc_B, calc_B_Args)
  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  V_all <- var(samples)


  ###
  # Total Effect Index of value (x) is  1 minus the variance effect of all
  # other variables.

  # Total Effect Index of X, i.e. fixed X
  set.seed(8121976) # just so tests wont fail for random number changes
  x <- ValueWithUncertaintyFixed(x)
  y <- ValueWithUncertaintySampled(y)
  z <- ValueWithUncertaintySampled(z)
  estA <- createEstValueModel(calc_A(x), calc_A, calc_A_Args)
  estB <- createEstValueModel(calc_B(y), calc_B, calc_B_Args)
  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  TEI_X <- 1 - (var(samples) / V_all)
  expect_equal(signif(TEI_X, 4), 0.7988)

  # Total Effect Index of Y, i.e. fixed Y
  set.seed(8121976) # just so tests wont fail for random number changes
  x <- ValueWithUncertaintySampled(x)
  y <- ValueWithUncertaintyFixed(y)
  z <- ValueWithUncertaintySampled(z)
  estA <- createEstValueModel(calc_A(x), calc_A, calc_A_Args)
  estB <- createEstValueModel(calc_B(y), calc_B, calc_B_Args)
  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  TEI_Y <- 1 - (var(samples) / V_all)
  expect_equal(signif(TEI_Y, 4), 0.008275)

  # Total Effect Index of Z, i.e. fixed Z
  set.seed(8121976) # just so tests wont fail for random number changes
  x <- ValueWithUncertaintySampled(x)
  y <- ValueWithUncertaintySampled(y)
  z <- ValueWithUncertaintyFixed(z)
  estA <- createEstValueModel(calc_A(x), calc_A, calc_A_Args)
  estB <- createEstValueModel(calc_B(y), calc_B, calc_B_Args)
  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  TEI_Z <- 1 - (var(samples) / V_all)
  expect_equal(signif(TEI_Z, 4), 0.2695)
})
