warn_once <- function(message, fun, type) {
  type <- paste0("ValueWithUncertainty.warn.", type)
  if (getOption(type)) {
    options(as.list(setNames(FALSE, type)))
    warning("In '", fun, "' : ", message, call. = FALSE)
  }
}

warn_once_bool <- function(fun) warn_once(
  "boolean operators not defined for 'ValueWithUncertainty' objects, uncertainty dropped",
  fun = fun,
  type = "bool"
)

warn_once_coercion <- function(fun) warn_once(
  "non-'ValueWithUncertainty' operand automatically coerced to an 'ValueWithUncertainty' object with no uncertainty",
  fun = "Ops",
  type = "coercion"
)

#.v <- function(x) as.numeric(x)

# get_exponent <- function(x) ifelse(.v(x), floor(log10(abs(.v(x)))), 0)

# cummatrix <- function(x, fill=0) {
#   t(sapply(seq_len(length(x)), function(lag) {
#     c(rep(fill, lag-1), x[1:(length(x)-lag+1)])
#   }))
# }

# cond2int <- function(...) {
#   args <- c(...)
#   sum(2^(seq_along(args) - 1) * args)
# }
