#' @title Put an object to cache
#' @description Save the object (dataset/toc/DSD) to cache
#' @param obj  an object (toc, dataset, DSD)
#' @param oname  a character string with the name of the object to reference later in the cache
#' @param update_cache a logical with a default value \code{FALSE}, whether to update the cache. In this case the existing value in the cahce is overwritten.  Can be set also with \code{options(restatapi_update = TRUE)}
#' @param cache_dir a path to a cache directory. The default is \code{NULL}, in this case the object is saved in the memory (in the '.restatapi_env'). Otherwise if the \code{cache_dir} directory does not exist it saves in the 'restatapi' directory under the temporary directory from \code{tempdir()}. Directory can also be set with \code{options(restatapi_cache_dir=...)}.
#' @param compress_file a logical whether to compress the RDS-file in caching. Default is \code{TRUE}.
#' @export 
#' @details Saves a given object in cache. This can be the memory \code{.restatapi_env} or on the hards disk. If the given \code{cache_dir} does not exist then the file is saved in the R temp directory (\code{tempdir()}. If the file or object with the \code{oname} exists in the cache, then the object is not cached. 
#' @return The function returns the place where the object was cached: either it creates an the object in the memory ('.restatapi_env') or creates an RDS-file.  
#' @examples 
#' \dontshow{
#' if ((parallel::detectCores()<2)|(Sys.info()[['sysname']]=='Windows')){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }    
#' options(restatapi_cache_dir=NULL)
#' }
#' dt<-data.frame(txt=c("a","b","c"),nr=c(1,2,3))
#' put_eurostat_cache(dt,"teszt")
#' get("teszt",envir=.restatapi_env)
#' 

put_eurostat_cache<-function(obj,oname,update_cache=FALSE,cache_dir=NULL,compress_file=TRUE){
  pl<-NULL
  if (is.null(cache_dir)){cache_dir <- getOption("restatapi_cache_dir", NULL)}
  if (is.null(cache_dir)){
    if (!exists(oname,envir=.restatapi_env)) {
      assign(oname,obj,envir=.restatapi_env)
      pl<-paste0("in memory ('",oname,"' in '.restatapi_env')")
    } else if (update_cache){
      assign(oname,obj,envir=.restatapi_env)
      pl<-paste0("in memory ('",oname,"' in '.restatapi_env'). The previous value was overwritten")
    } else {
      pl<-paste0("previously in memory ('",oname,"' in '.restatapi_env'). It remained unchanged")
    }
  } else if (dir.exists(cache_dir)){
    fname<-file.path(sub("[\\/]$","",cache_dir,perl=TRUE),paste0(oname,".rds"))
    if (!file.exists(fname)){
      saveRDS(obj,file=fname,compress=compress_file)
      pl<-paste("in the file", fname)
    }else if(update_cache){
      saveRDS(obj,file=fname,compress=compress_file)
      pl<-paste0("previously in the file ",fname,". The previous value is now overwritten")
    }else{
      pl<-paste("previously in the file",fname)
    }
  }else{
    if (!dir.exists(file.path(tempdir(),"restatapi"))){dir.create(file.path(tempdir(),"restatapi"))}
    fname<-file.path(tempdir(),"restatapi",paste0(oname,".rds"))
    if (!file.exists(fname)){
      saveRDS(obj,file=fname,compress=compress_file)
      pl<-paste("in the file", fname)
    }else if(update_cache){
      saveRDS(obj,file=fname,compress=compress_file)
      pl<-paste0("previously in the file ",fname,". The previous value is now overwritten")
    }else{
      pl<-paste("previously in the file",fname)
    }
  }
  return(pl)
}