#' @import data.table
.onLoad <- function(libname, pkgname){
  load_cfg(verbose=TRUE)
  options(restatapi_verbose=FALSE)
}
