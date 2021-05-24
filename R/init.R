.onLoad <- function(libname, pkgname) {
  types <- c("bool", "coercion", "matmult")
  types <- paste0("ValueWithUncertainty.warn.", types)
  options(as.list(setNames(rep.int(TRUE, length(types)), types)))
}
