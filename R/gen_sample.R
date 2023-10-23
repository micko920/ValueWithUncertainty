#' Method for generating samples of uncertain inputs
#'
#' HOLD Generate a sample of inputs base on the model
#'
#' @references TBC
#'
#' @param fixed Disable model and return value, default is false
#' @return Returns a sample of values based on the model
#' @export
gen_sample <- function(v, n = 1, fixed = FALSE, ...) UseMethod("gen_sample")


#' Method for generating samples of uncertainty inputs
#'
#' HOLD Generate a sample of inputs with uncertainty based on the model
#'
#' @references TBC
#' @param fixed Disable model and return value, default is false
#' @return HOLD Returns a sample of values with uncertainty based on the model
#' @export
gen_sample.ValueWithUncertainty <- function(v, n = 1, fixed = FALSE, ...) {
  theta <- ValueWithUncertaintyValue(v)
  lower <- min(v)
  upper <- max(v)

  #if (!(lower < theta && theta < upper)) {
  #	print(v)
  #	print(c(lower,theta,upper))
  #	browser()
  #	stop("assert lower < theta < upper values are required", call. = FALSE)
  #}
  if (fixed) {
    return(rep(ValueWithUncertaintyValue(v), n))
  } else {
    return(do.call(attr(v, "model"), list(v = v, n = n, ...)))
  }
}
