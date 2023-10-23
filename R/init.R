# nocov added because .onload cannot be tested
.onLoad <- function(libname, pkgname) {# nocov start
  types <- c("bool", "coercion", "matmult")
  types <- paste0("ValueWithUncertainty.warn.", types)
  options(as.list(setNames(rep.int(TRUE, length(types)), types)))
}# nocov end
