#' @import data.table
.onLoad <- function(libname, pkgname){
  options(mc.cores=parallel::detectCores()-1)
  load_cfg(verbose=T)
  options(restatapi_verbose=F)
}

