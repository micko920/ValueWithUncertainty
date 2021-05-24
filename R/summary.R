#' Value With Uncertainty Summary 
#' 
#' Summary of Value with Uncertainty Export Functions
#'
#' @references [TBC] 
#'  
#' @rdname groupGeneric.ValueWithUncertainty
#'
#' @details \subsection{\code{Summary}}{
#' The methods \code{all} and \code{any} are not supported for \code{ValueWithUncertainty}
#' objects and fail with an informative message. \code{min}, \code{max} (and
#' \code{range}) return the minimum or (and) maximum of the CI
#'
#' @examples
#' c(min(x), max(x))
#' range(x)
#' sum(y)
#' prod(y)
#' @export
Summary.ValueWithUncertainty <- function(..., na.rm = FALSE) {
  dots <- list(...)
  dots[names(dots) != ""] <- NULL # unnamed only
  x <- do.call(c, dots)
  switch(.Generic,
    "all" = ,
    "any" =
      stop("method not supported for `ValueWithUncertainty` objects"),
    "sum" = NextMethod(),
    "prod" = NextMethod(),
    "max" = ValueWithUncertaintyMax(x),
    "min" = ValueWithUncertaintyMin(x),
    "range" = range(ValueWithUncertaintyMin(x), ValueWithUncertaintyMax(x), na.rm = na.rm)
  )
}

#' @export
max.ValueWithUncertainty <- function(x, ...) ValueWithUncertaintyMax(x)

#' @export
min.ValueWithUncertainty <- function(x, ...) ValueWithUncertaintyMin(x)

#' @export
range.ValueWithUncertainty <- function(x, ...) range(ValueWithUncertaintyMin(x), ValueWithUncertaintyMax(x))

#' @export
mean.ValueWithUncertainty <- function(x, ...) ValueWithUncertaintyValue(x)

#' TODO:export
# weighted.mean.ValueWithUncertainty <- function(x, ...) weighted.mean(x, ...)

#' @export
median.ValueWithUncertainty <- function(x, ...) 1.253 * mean(x, ...)

#' TODO:export
# quantile.ValueWithUncertainty <- function(x, ...) quantile(unclass(x), ...)

#' @export
summary.ValueWithUncertainty <- function(x, ..., digits = max(3L, getOption("digits") - 3L)) {
    qq <- signif(c(ValueWithUncertaintyMin(x), as.numeric(x),
                     ValueWithUncertaintyMax(x)), digits)
	names(qq) <- c("Lower CI", "Value", "Upper CI")
	qq
}
