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

check_ctor <- function(LowerCI, Value, UpperCI, model) {
  if (!is.numeric(Value)) stop("must be numeric", call. = FALSE)
  if (!is.numeric(LowerCI)) stop("must be numeric", call. = FALSE)
  if (!is.numeric(UpperCI)) stop("must be numeric", call. = FALSE)
  ifelse((LowerCI != as.numeric(Value))
         && (LowerCI > as.numeric(Value)), stop("LowerCI must be lower than Value", call. = FALSE),NA)
  ifelse((UpperCI != as.numeric(Value))
         && (UpperCI < as.numeric(Value)), stop("UpperCI must be higher than Value", call. = FALSE),NA)
  m <- ifelse(LowerCI == as.numeric(Value) && UpperCI == as.numeric(Value), vwuFixed, model)
  return(m)
}
