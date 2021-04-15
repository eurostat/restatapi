#' @title Load an object from cache
#' @description Search and load the object (dataset/toc/DSD) from cache
#' @param oname  a character string with the name of the object (toc, dataset id, DSD id)
#' @param cache_dir a path to a cache directory to search in. The default is \code{NULL}, 
#'        in this case the object is searched in the memory (in the \code{.restatapi_env}). Otherwise
#'        if the \code{cache_dir} directory does not exist it searches the 'restatapi' directory
#'        in the temporary directory from \code{tempdir()}. Directory can also be set with \code{options(restatapi_cache_dir=...)}.
#' @param verbose a logical value with default \code{FALSE}, so detailed messages (for debugging) will not printed.
#'         Can be set also with \code{options(restatapi_verbose=TRUE)}.
#' @export
#' @details If the given name or the beginning of the name (for datasets) found in the cache then it returns the value of the object otherwise it returns \code{NULL}.
#' @return the requested object if exists in the '.restatapi_env' or in the \code{cache_dir}, otherwise it returns the \code{NULL} value.  
#' @examples 
#' \dontshow{
#' if (parallel::detectCores()<=2){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }    
#' }
#' dt<-data.frame(txt=c("a","b","c"),nr=c(1,2,3))
#' put_eurostat_cache(dt,"teszt")
#' get_eurostat_cache("teszt",verbose=TRUE)
#' 
#' 

get_eurostat_cache<-function(oname,cache_dir=NULL,verbose=FALSE){
  obj<-oname_p<-NULL
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  if (is.null(cache_dir)){cache_dir <- getOption("restatapi_cache_dir", NULL)}
  oname_all<-unique(c(oname,
                      sub("-0-0$","-1-1",oname),
                      sub("-0-0$","-1-0",oname),
                      sub("-1-0$","-1-1",oname),
                      sub("-0-0-","-1-1-",oname),
                      sub("-0-0-","-1-0-",oname),
                      sub("-1-0-","-1-1-",oname),
                      sub("(.*\\.\\d\\d)-0$","\\1-1",oname)))
  if(length(gregexpr("-", oname)[[1]])>2){
    oname_p<-sapply(c(1:(length(gregexpr("-", oname)[[1]])-2)),FUN=pgen,oname=oname)
    oname_p<-unique(c(oname_p,
                      sub("^b(.*\\.\\d\\d)-0$","r\\1-1",oname_p),
                      sub("-0-0$","-1-1",oname_p),
                      sub("-0-0$","-1-0",oname_p),
                      sub("-1-0$","-1-1",oname_p),
                      sub("^b(.*\\.\\d\\d-[0|1])$","r\\1",oname_p,perl=TRUE)))
  }
  oname_all<-unique(c(oname_all,oname_p))
  if (any(sapply(oname_all,exists,envir=restatapi::.restatapi_env))){
    if (verbose) {message("The '",oname,"' was loaded from '",oname_all[sapply(oname_all,exists,envir=restatapi::.restatapi_env)][1],"' in '.restatapi_env'.")}
    return(get(oname_all[sapply(oname_all,exists,envir=restatapi::.restatapi_env)][1],envir=restatapi::.restatapi_env))
  } else if (!is.null(cache_dir)){
    if (dir.exists(cache_dir)){
      fname<-file.path(sub("[\\/]$","",cache_dir,perl=TRUE),paste0(oname_all,".rds"))
      if (any(file.exists(fname))){
        if (verbose) {message("The '",oname,"' was loaded from ",fname[file.exists(fname)][1],".")}
        return(readRDS(fname[file.exists(fname)][1])[])  
      } 
    } else {
      fname<-file.path(tempdir(),"restatapi",paste0(oname_all,".rds"))
      if (any(file.exists(fname))){
        if (verbose) {message("The '",oname,"' was loaded from ",fname[file.exists(fname)][1],".")}
        return(readRDS(fname[file.exists(fname)][1])[])
      } else {
        return(NULL)
      }
    } 
  } else {
    return(NULL)
  }  
}

pgen<-function(x,oname){
  sub(paste0("((?:-[^-rb\r\n]*){",x,"})$"),"",oname,perl=TRUE)
}