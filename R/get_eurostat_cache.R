#' @title Load an object from cache
#' @description Search and load the object (dataset/toc/DSD) from cache
#' @param oname  a character string with the name of the object (toc, dataset id, DSD id)
#' @param cache_dir a path to a cache directory to search in. The default is \code{NULL}, 
#' in this case the object is searched in the memory (in the '.restatapi_env'). Otherwise
#'  if the \code{cache_dir} directory does not exist it searches the 'restatapi' directory
#'   in the temporary directory from \code{tempdir()}. Directory can also be set with \code{options(restatapi_cache_dir=...)}.
#' @export
#' @details If the given name or the beginning of the name (for datasets) found in the cache then it returns the value of the object otherwise it returns \code{NULL}.
#' @return The requested object if exists in the '.restatapi_env' or in the \code{cache_dir}, otherwise it returns the \code{NULL} value.  
#' @examples 
#' \dontshow{
#' options(mc.cores=min((parallel::detectCores()),2))
#' }
#' toc<-get_eurostat_toc(cache=TRUE)
#' head(get_eurostat_cache("toc.xml"))
#' 

get_eurostat_cache<-function(oname, cache_dir=NULL){
  obj<-oname_p<-NULL
  if (is.null(cache_dir)){cache_dir <- getOption("restatapi_cache_dir", NULL)}
  oname_all<-unique(c(oname,sub("-0$","-1",oname),sub("-0-","-1-",oname),sub("^b_","r_",oname,perl=T)))
  if(length(gregexpr("-", oname)[[1]])>2){
    oname_p<-sapply(c(1:(length(gregexpr("-", oname)[[1]])-2)),FUN=pgen,oname=oname)
    oname_p<-unique(c(oname_p,sub("-0$","-1",oname_p),sub("-0-","-1-",oname_p),sub("^b_","r_",oname_p,perl=T)))
  }
  oname_all<-unique(c(oname_all,oname_p))
  if (any(sapply(oname_all,exists,envir=.restatapi_env))){
    get(oname_all[sapply(oname_all,exists,envir=.restatapi_env)][1], envir = .restatapi_env)
    #eval(parse(text=(paste0(".restatapi_env$",oname_all[sapply(oname_all,exists,envir=.restatapi_env)][1]))))
  } else if (!is.null(cache_dir)){
    if (dir.exists(cache_dir)){
      fname<-file.path(sub("[\\/]$","",cache_dir,perl=T),paste0(oname_all,".rds"))
      if (any(file.exists(fname))){
        readRDS(fname[file.exists(fname)][1])  
      } 
    } else {
      fname<-file.path(tempdir(),"restatapi",paste0(oname_all,".rds"))
      if (any(file.exists(fname))){
        readRDS(fname[file.exists(fname)][1])  
      } else {
        NULL
      }
    } 
  } else {
    NULL
  }  
}

pgen<-function(x,oname){
  sub(paste0("((?:-[^-rb\r\n]*){",x,"})$"),"",oname,perl=T)
}