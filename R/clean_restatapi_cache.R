#' @title Clean restatapi Cache
#' @description  Remove all objects from the .restatapi_env except the configuration file and delete all .rds files from the default and selected cache directory.
#'              See \code{\link{get_eurostat_data}} for more on cache.
#' @param cache_dir A path to cache directory. If \code{NULL} (default)
#'        tries to clean default temporary cache directory. Directory can also be set with \code{options(restatapi_cache_dir=...)}.
#' @param verbose A boolean with default \code{FALSE}, so detailed messages (for debugging) will not printed. Can be set also with
#'        \code{options(restatapi_verbose=TRUE)}
#' @export
#' @examples 
#' \dontrun{
#' clean_restatapi_cache() 
#' }

clean_restatapi_cache<-function(cache_dir=NULL,verbose=FALSE){
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  if (is.null(cache_dir)){cache_dir<-getOption("restatapi_cache_dir",NULL)}
  files<-cdirs<-NULL
  if (!(exists(".restatapi_env"))) {
    load_cfg()
  } else {
    td<-ls(envir=.restatapi_env)
    td<-td[!(td %in% c("cfg","rav"))]
    rm(list=td,envir=.restatapi_env)
  }
  if (!is.null(cache_dir)){
    if (dir.exists(cache_dir)){
      cdirs<-cache_dir
      files<-list.files(cache_dir,pattern=".rds",full.names=TRUE)
    }  
  }
  cache_dir<-file.path(tempdir(),"restatapi")
  if (dir.exists(cache_dir)){
    cdirs<-c(cdirs,cache_dir)
    files<-unique(c(files,list.files(cache_dir,pattern=".rds",full.names=TRUE)))
  }
  
  if (!is.null(files)){
    if (length(files) == 0) {
      if (verbose){message("The cache folder ",cache_dir," is empty.")}
    } else {
      unlink(files)
      if (verbose){message("Deleted all .rds files from ",paste(cdirs,collapse="; "))}    
    }  
  }
}

