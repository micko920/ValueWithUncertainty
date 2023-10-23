#' Ops Value with Uncertainty
#'
#' Ops
#'
#' @references [TBC]
#'
#' @param e1 Parameter definition tbc
#' @param e2 Parameter definition tbc
#' @return tbc output from function
#' @rdname groupGeneric.ValueWithUncertainty
#'
#' @details \subsection{\code{Ops}}{
#' Boolean operators drop the uncertainty (showing a warning once) and operate on the
#' numeric values.
#' Any \code{ValueWithUncertainty} operand is automatically coerced to numeric
#' with no uncertainty.}
#'
#' @examples
#' y <- ValueWithUncertainty(1, 10, 100)
#' x / sqrt(y) + y * sin(x)
#'
#' # numeric values automatically coerced this type to numeric and drop
#' # uncertainty
#' x^2
#'
#' # boolean operators drop uncertainty
#' y > x
#' @export
Ops.ValueWithUncertainty <- function(e1, e2) {
  if (.Generic %in% c("&", "|", "!", "==", "!=", "<", ">", "<=", ">=")) {
    warn_once_bool(.Generic)
    return(NextMethod())
  }

  if (!missing(e1)) {
    if (inherits(e1, "ValueWithUncertainty")) {
      e1 <- gen_sample(e1, fixed = attr(e1, "fixed"))
    }
  }

  if (!missing(e2)) {
    if (inherits(e2, "ValueWithUncertainty")) {
      e2 <- gen_sample(e2, fixed = attr(e2, "fixed"))
    }
  }

  return(NextMethod())
}
