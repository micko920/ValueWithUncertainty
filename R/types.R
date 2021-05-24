#' Coerce to a numeric
#'
#' S3 method for \code{ValueWithUncertainty} objects
#'
#' @inheritParams base::as.numeric
#'
#' @examples
#'

as.double.ValueWithUncertainty <- function(x, ...) {
  return (gen_sample(x, fixed = attr(x, "fixed")))
}
