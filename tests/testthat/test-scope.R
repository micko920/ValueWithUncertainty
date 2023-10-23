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

  test_aval <- do.call(calc_A, calc_A_Args())
  test_bval <- do.call(calc_B, calc_B_Args())
  test_cval <- calc_C(x,y,z)


  test_c_args <- as.numeric(calc_C_Args())
  estC <- calc_C(estA, estB, z)


  # No variation
  expect_equal((x + y) / z,test_cval)
  expect_equal(calc_C(x,y,z),test_cval)

  expect_equal(gen_sample(x,1,TRUE), c(xval))
  expect_equal(gen_sample(y,1,TRUE), c(yval))
  expect_equal(gen_sample(z,1,TRUE), c(zval))
  expect_equal(as.numeric(x), c(xval))
  expect_equal(as.numeric(y), c(yval))
  expect_equal(as.numeric(z), c(zval))
  expect_equal(sapply(list(x,y,z),as.numeric),c(xval,yval,zval))
  expect_equal(sapply(calc_C_Args(),as.numeric),test_c_args)

  expect_equal(do.call(calc_A, calc_A_Args()),test_aval)
  expect_equal(do.call(calc_B, calc_B_Args()),test_bval)
  expect_equal(do.call(calc_C, calc_C_Args()),estC)

  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  expect_true(isTRUE(length(unique(samples)) == 1))
  expect_equal(signif(mean(samples),3), signif(estC,3))

  set.seed(8121976) # just so tests wont fail for random number changes
  # sample values for z
  x <- ValueWithUncertaintyFixed(x)
  y <- ValueWithUncertaintyFixed(y)
  z <- ValueWithUncertaintySampled(z)
  expect_equal(as.numeric(x), c(xval))
  expect_equal(as.numeric(y), c(yval))
  expect_equal(as.numeric(z), c(68))

  expect_equal(sapply(list(x,y,z),as.numeric),c(xval,yval,117))
  expect_equal(sapply(calc_C_Args(),as.numeric),c(test_c_args[1:2], 118))

  expect_equal(signif(as.numeric((x + y) / z),3),signif(0.14,digits=3))
  expect_equal(signif(calc_C(x,y,z),3),signif(0.110,3))
  expect_false(isTRUE(all.equal(signif(test_cval,3),signif(0.110,3))))

  expect_equal(do.call(calc_A, calc_A_Args()),test_aval)
  expect_equal(do.call(calc_B, calc_B_Args()),test_bval)
  expect_equal(signif(do.call(calc_C, calc_C_Args()),3),signif(0.1103,3))

  expect_false(isTRUE(all.equal(signif(estC,3),signif(0.372,3))))
  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  expect_false(isTRUE(length(unique(samples)) == 1))



  x <- ValueWithUncertaintyFixed(x)
  y <- ValueWithUncertaintyFixed(y)
  z <- ValueWithUncertaintyFixed(z)
  expect_equal(gen_sample(x,1,TRUE), c(xval))
  expect_equal(gen_sample(y,1,TRUE), c(yval))
  expect_equal(gen_sample(z,1,TRUE), c(zval))
  expect_equal(as.numeric(x), c(xval))
  expect_equal(as.numeric(y), c(yval))
  expect_equal(as.numeric(z), c(zval))
  expect_equal((x + y) / z,test_cval)
  expect_equal(calc_C(x,y,z),test_cval)

  expect_equal(sapply(list(x,y,z),as.numeric),c(xval,yval,zval))
  expect_equal(sapply(calc_C_Args(),as.numeric),test_c_args)

  expect_equal(do.call(calc_A, calc_A_Args()),test_aval)
  expect_equal(do.call(calc_B, calc_B_Args()),test_bval)
  expect_equal(do.call(calc_C, calc_C_Args()),estC)

  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  expect_true(isTRUE(length(unique(samples)) == 1))
  expect_equal(signif(mean(samples),3), signif(estC,3))

  set.seed(8121976) # just so tests wont fail for random number changes
  # sample values for z
  x <- ValueWithUncertaintyFixed(x)
  y <- ValueWithUncertaintySampled(y)
  z <- ValueWithUncertaintyFixed(z)
  expect_equal(as.numeric(x), c(xval))
  expect_equal(as.numeric(y), c(1))
  expect_equal(as.numeric(z), c(zval))
  expect_equal(signif(as.numeric((x + y) / z),3),signif(0.13,digits=3))

  expect_equal(sapply(list(x,y,z),as.numeric),c(xval,3,zval))
  expect_equal(sapply(calc_C_Args(),as.numeric),test_c_args)

  expect_equal(do.call(calc_A, calc_A_Args()),test_aval)
  expect_equal(do.call(calc_B, calc_B_Args()),11)
  expect_equal(do.call(calc_C, calc_C_Args()),estC)

  expect_equal(sapply(calc_B_Args(),as.numeric),2)
  expect_equal(sapply(calc_B_Args(),as.numeric),3)
  expect_equal(as.numeric(estB), 11)
  expect_equal(as.numeric(estB), 11)
  expect_equal(as.numeric(estB), 11)
  expect_equal(as.numeric(estB), 11)


  estB <- ValueWithUncertaintySampled(estB)

  expect_equal(as.numeric(estB), 12)
  expect_equal(as.numeric(estB), 12)
  expect_equal(as.numeric(estB), 11)
  expect_equal(as.numeric(estB), 11)
  expect_equal(sapply(calc_B_Args(),as.numeric),2)
  expect_equal(sapply(calc_C_Args(),as.numeric), c(test_c_args[1], 10, test_c_args[3]))

  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  expect_false(isTRUE(length(unique(samples)) == 1))


  set.seed(8121976) # just so tests wont fail for random number changes
  x <- ValueWithUncertaintyFixed(x)
  y <- ValueWithUncertaintyFixed(y)
  z <- ValueWithUncertaintyFixed(z)
  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  expect_true(isTRUE(length(unique(samples)) == 1))

  ###
  # Let Y be sampled
  set.seed(8121976) # just so tests wont fail for random number changes
  x <- ValueWithUncertaintyFixed(x)
  y <- ValueWithUncertaintySampled(y)
  z <- ValueWithUncertaintyFixed(z)
  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))
  expect_false(isTRUE(length(unique(samples)) == 1))

  compare_summary_equal(samples, 0.1400, 0.1500, 0.1600, 0.1600, 0.1700, 0.1800)



  ###
  # Let Z be sampled
  set.seed(8121976) # just so tests wont fail for random number changes
  x <- ValueWithUncertaintyFixed(x)
  y <- ValueWithUncertaintyFixed(y)
  z <- ValueWithUncertaintySampled(z)
  samples <- replicate(10000, do.call(calc_C, calc_C_Args()))

  compare_summary_equal(samples, 0.107, 0.128, 0.160, 0.176, 0.213, 0.320)


})

