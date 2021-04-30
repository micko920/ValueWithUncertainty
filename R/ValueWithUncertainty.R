#' ValueWithUncertainty
#'
#' `ValueWithUncertainty` does something.
#'
#' @param p A value to report
#' @return Returns string of the value \code{p}
#' @param digits The number of decimal places
#' @return Returns p with \code{digits} decimal places
#' @examples
#'
#' ValueWithUncertainty(1) # returns "1.000"
#'
#' @export

ValueWithUncertainty <- function(p, digits = 3) {
  if (p < 0) stop("p cannot be less than 0")
  if (p > 1) stop("p cannot be greater than 1")

  if (!(digits %in% 1:5)) {
    warning("digits should probably be an integer between 1 and 5")
    digits = 3
  }

  if (p < .001) return("p < .001")

  p_round <- round(p, digits) %>%
    as.character() %>%
    # omit leading zero for APA-style
    stringr::str_replace("0.", ".") %>%
    # pad right with zeros
    stringr::str_pad(digits+1, "right", 0)

  p_string <- paste("p =", p_round)

  return(p_string)
}
