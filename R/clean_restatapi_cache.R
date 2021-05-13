#' @title Clean restatapi cache
#' @description Remove all objects from the \code{.restatapi_env} except the configuration file, API version number, download method and the country codes.
#'              In addition, it deletes all the .rds files from the default and selected cache directory.
#'              See \code{\link{get_eurostat_data}} for more on cache.
#' @param cache_dir a path to cache directory. If \code{NULL} (default) it will clean default temporary cache directory (\code{file.path(tempdir(),"restatapi")}). The default 
#'              cache directory is used when the provided \code{cache_dir} does not exist. Directory can also be set with \code{options(restatapi_cache_dir=...)}.
#' @param verbose a logical value with default \code{FALSE}, so detailed messages (for debugging) will not be printed. Can be set also with
#'        \code{options(restatapi_verbose=TRUE)}
#' @export
#' @examples 
#' 
#' clean_restatapi_cache(verbose=TRUE) 
#' 

clean_restatapi_cache<-function(cache_dir=NULL,verbose=FALSE){
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  if (is.null(cache_dir)){cache_dir<-getOption("restatapi_cache_dir",NULL)}
  files<-cdirs<-NULL
  if (!(exists(".restatapi_env"))) {
    load_cfg()
  } else {
    td<-ls(envir=restatapi::.restatapi_env)
    td<-td[!(td %in% c("cfg","rav","cc","dmethod"))]
    rm(list=td,envir=restatapi::.restatapi_env)
    if (verbose){message("clean_restatapi_cache - All objects except from 'cfg', 'rav', 'cc' and 'dmethod' are removed from '.restatapi_env'.")}
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
    files<-c(files,cache_dir)
  }
  
  if (!is.null(files)){
    if (length(files) == 0) {
      if (verbose){message("clean_restatapi_cache - The cache folder ",cache_dir," is empty.")}
    } else {
      unlink(files,recursive=TRUE,force=TRUE)
      if (verbose){message("clean_restatapi_cache - Deleted all .rds files from ",paste(cdirs,collapse="; "))}    
    }  
  }
  
}

