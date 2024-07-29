#' ValueWithUncertainty
#'
#' `ValueWithUncertainty` returns an object which represents a value with a CI
#'
#' @param p A value to report
#' @return Returns string of the value \code{p}
#' @param digits The number of decimal places
#' @return Returns p with \code{digits} decimal places
#'
#'



#' ValueWithUncertainty(1) # returns object
#' @export
ValueWithUncertainty <- function(LowerCI, Value, UpperCI, model = as.numeric, fixed = TRUE, name = "default") {
  if (!is.numeric(Value)) stop("Value must be numeric", call. = FALSE)
  if (!is.numeric(LowerCI)) stop("LowerCI must be numeric", call. = FALSE)
  if (!is.numeric(UpperCI)) stop("UpperCI must be numeric", call. = FALSE)
  if (any(LowerCI > Value)) stop("LowerCI must be lower than Value", call. = FALSE)
  if (any(UpperCI < Value)) stop("UpperCI must be higher than Value", call. = FALSE)
  v <- as.numeric(Value)
  if (inherits(Value, "ValueWithUncertainty")) {
    v <- attr(Value, "value")
  }
  new_obj <- structure(v,
    value = v,
    lci = LowerCI, uci = UpperCI, model = model,
    fixed = fixed, symbol = name, class = "ValueWithUncertainty"
  )
  if (length(new_obj) == 1) return(simplify2array(new_obj)[[1]])
  return(simplify2array(new_obj))
}

#' @export
ValueWithUncertaintyValue <- function(x) UseMethod("ValueWithUncertaintyValue")

#' @export
ValueWithUncertaintyValue.ValueWithUncertainty <- function(x) attr(x, "value")

#' @export
ValueWithUncertaintyName <- function(x) UseMethod("ValueWithUncertaintyName")

#' @export
ValueWithUncertaintyName.ValueWithUncertainty <- function(x) attr(x, "symbol")


#' @export
ValueWithUncertaintyMax <- function(x) UseMethod("ValueWithUncertaintyMax")

#' @export
ValueWithUncertaintyMax.ValueWithUncertainty <- function(x) attr(x, "uci")

#' @export
ValueWithUncertaintyMin <- function(x) UseMethod("ValueWithUncertaintyMin")

#' @export
ValueWithUncertaintyMin.ValueWithUncertainty <- function(x) attr(x, "lci")

#' @export
ValueWithUncertaintyModel <- function(x) UseMethod("ValueWithUncertaintyModel")

#' @export
ValueWithUncertaintyModel.ValueWithUncertainty <- function(x) attr(x, "model")

#' @export
ValueWithUncertaintySampled <- function(x) UseMethod("ValueWithUncertaintySampled")

#' @export
ValueWithUncertaintySampled.ValueWithUncertainty <- function(x) {
  attr(x, "fixed") <- FALSE
  return(x)
}

#' @export
ValueWithUncertaintySampled.list <- function(x, ...) {
  return (sapply(x,ValueWithUncertaintySampled,simplify = FALSE,...))
}


#' @export
ValueWithUncertaintyFixed <- function(x) UseMethod("ValueWithUncertaintyFixed")

#' @export
ValueWithUncertaintyFixed.ValueWithUncertainty <- function(x) {
  attr(x, "fixed") <- TRUE
  return(x)
}


#' @export
ValueWithUncertaintyFixed.list <- function(x, ...) {
  return (sapply(x,ValueWithUncertaintyFixed,simplify = FALSE,...))
}




##### Value Models #####


#' @export
vwuNorm <- function(v, n, ...) {
  QUCI <- get0("QUIC", ifnotfound = 0.95)
  sd <- (diff(range(v)) / (2 * qnorm(p = QUCI))) # LCI,UCI are 90% QCI (5%,95%)
  return(rnorm(n, mean = ValueWithUncertaintyValue(v), sd = sd))
}

#' @export
vwuTriangle <- function(v, n, ...) {
  theta <- ValueWithUncertaintyValue(v)
  lower <- min(v)
  upper <- max(v)
  if (!(lower < theta && theta < upper)) {
	print(v)
	print(c(lower,theta,upper))
	browser()
	stop("assert lower < theta < upper values are required", call. = FALSE)
  }
  return(rtriangle(n = n, theta = theta, lower = min(v), upper = max(v)))
}

#' @export
create_vwuSampled <- function(value_samples) {
  return(function(v, n, ...) {
    return(sample(value_samples, size = n, replace = TRUE))
  })
}

#' @export
vwuFixed <- function(v, n, ...) {
  return(rep(ValueWithUncertaintyValue(v), n))
}
