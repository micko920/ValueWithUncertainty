#' Coerce to a numeric
#'
#' S3 method for \code{ValueWithUncertainty} objects
#'
#' @inheritParams base::as.numeric
#'
#'
#'

#' @export
as.double.ValueWithUncertainty <- function(x, ...) {
  return (gen_sample(x, fixed = attr(x, "fixed")))
}

#' @export
as.numeric.ValueWithUncertainty <- function(x, ...) {
  return (gen_sample(x, fixed = attr(x, "fixed")))
}

