#' ValueWithUncertainty
#'
#' `ValueWithUncertainty` returns an object which represents a value with a CI
#'
#' @param p A value to report
#' @return Returns string of the value \code{p}
#' @param digits The number of decimal places
#' @return Returns p with \code{digits} decimal places
#' @examples
#'
#'



#' ValueWithUncertainty(1) # returns object
#' @export
ValueWithUncertainty <- function(LowerCI, Value, UpperCI, model = function(v, n = 1) { attr(v,"value") }, fixed = TRUE) {
  new_obj <- apply(
    cbind(LowerCI,Value, UpperCI),1,
    function(p) {
      m <- check_ctor(p[[1]],p[[2]],p[[3]],model)
      if (inherits(p[[2]], "ValueWithUncertainty")) {
        v <- attr(p[[2]], "value")
      } else v <- p[[2]]
      return(list(structure(as.numeric(p[[2]]),
        value = v,
        lci = p[[1]], uci = p[[3]], model = m,
        fixed = fixed, class = "ValueWithUncertainty"
      )))
    }
  )
  if (length(new_obj) == 1) return(simplify2array(new_obj)[[1]])
  return(simplify2array(new_obj))
}

#' @export
ValueWithUncertaintyValue <- function(x) UseMethod("ValueWithUncertaintyValue")

#' @export
ValueWithUncertaintyValue.ValueWithUncertainty <- function(x) attr(x, "value")

#' @export
ValueWithUncertaintyValue.list <- function(x, ...) {
  return (sapply(x,ValueWithUncertaintyValue,simplify = FALSE,...))
}


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
