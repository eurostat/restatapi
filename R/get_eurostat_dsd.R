#' @title Download the Data Structure Definition of a dataset
#' @description Download Data Structure Definition (DSD) of a Eurostat dataset if it is not cached previously. 
#' @param id a character string with the id of the dataset. It is the value from the codename column of the \code{\link{get_eurostat_toc}} function. 
#' @param cache a boolean whether to load/save the TOC from/in the cache or not. The default value is \code{TRUE}, so that the TOC is checked first in the cache and if does not exist then downloaded from Eurostat and cached.
#' @param update_cache a boolean to update cache or not. The default value is \code{FALSE}, so the cache is not updated. Can be set also with \code{options(restatapi_update=TRUE)}
#' @param cache_dir a path to a cache directory. The default is \code{NULL}, in this case the TOC is cached in the memory (in the '.restatapi_env'). Otherwise if the \code{cache_dir} directory does not exist it creates the 'restatapi' directory in the temporary directory from \code{tempdir()} to save the RDS-file. Directory can also be set with \code{option(restatapi_cache_dir=...)}.
#' @param compress_file a logical whether to compress the RDS-file in caching. Default is \code{TRUE}.
#' @param verbose A boolean with default \code{FALSE}, so detailed messages (for debugging) will not printed.
#'         Can be set also with \code{options(restatapi_verbose=TRUE)}
#' @param ... parameter to pass on the \code{load_cfg} function
#' @return If the DSD does not exist it returns \code{NULL} otherwise the result is a table with the 3 columns:
#'    \tabular{ll}{
#'      \code{concept} \tab The name of the concepts in the order of the data structure \cr
#'      \code{code} \tab The possible list of codes under the concept \cr
#'      \code{name} \tab The name/description of the code 
#'    }
#' @export
#' @seealso \code{\link{get_eurostat_data}}, \code{\link{search_eurostat_toc}}.
#' @details The DSD is downloaded from Eurostat's website, through the REST API in XML (SDMX) format.
#'  
#' @references For more information see the detailed documentation of the \href{https://ec.europa.eu/eurostat/data/web-services}{API}. 
#' @examples 
#' \dontshow{
#' if (parallel::detectCores()<=2){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }
#' }
#' dsd<-get_eurostat_dsd("nama_10_gdp",cache=FALSE,verbose=TRUE)
#' head(dsd)
#' 

get_eurostat_dsd <- function(id,
                             cache=TRUE,
                             update_cache=FALSE,
                             cache_dir=NULL,
                             compress_file=TRUE,
                             verbose=FALSE,...) {
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  dmethod<-getOption("restatapi_dmethod",get("dmethod",envir=restatapi::.restatapi_env))
  if (is.null(id)){
    warning('No dataset id were provided.')
    dsd<-NULL
  } else {
    dsd<-dsd_xml<-NULL
    if((!exists(".restatapi_env")|(length(list(...))>0))){
      if ((length(list(...))>0)) {
        if (all(names(list(...)) %in% c("api_version","load_toc","parallel","max_cores","verbose"))){
          load_cfg(...)  
        } else {
          load_cfg()
        }
      } else {
        load_cfg()
      }  
    }
    update_cache <- update_cache | getOption("restatapi_update", FALSE)
    if ((cache) & (!update_cache)) {
      dsd<-get_eurostat_cache(paste0(id,".dsd"),cache_dir,verbose=verbose)
    }
    if ((!cache)|(is.null(dsd))|(update_cache)){
      cfg<-get("cfg",envir=restatapi::.restatapi_env) 
      rav<-get("rav",envir=restatapi::.restatapi_env)
      dsd_endpoint <- paste0(eval(parse(text=paste0("cfg$QUERY_BASE_URL$'",rav,"'$ESTAT$data$'2.1'$datastructure"))),"/DSD_",id)
      temp<-tempfile()
      if (verbose) {
        message("get_eurostat_dsd - Trying to download the DSD from: ",dsd_endpoint)
        tryCatch({utils::download.file(dsd_endpoint,temp,dmethod)},
                 error = function(e) {
                   message("get_eurostat_dsd - Error by the download of the DSD file:",'\n',paste(unlist(e),collapse="\n"))
                 },
                 warning = function(w) {
                   message("get_eurostat_dsd - Warning by the download of the DSD file:",'\n',paste(unlist(w),collapse="\n"))
                  })
        if (file.size(temp)!=0) {
          message("Trying to extract the DSD from: ",temp)
          tryCatch({dsd_xml<-xml2::read_xml(temp)},
                 error = function(e) {
                   message("get_eurostat_dsd - Error during the extraction of the XML from the downloaded DSD file:",'\n',paste(unlist(e),collapse="\n"))
                   dsd_xml<-NULL
                 },
                 warning = function(w) {
                   message("get_eurostat_dsd - There is warning by the extraction of the XML from the downloaded DSD file:",'\n',paste(unlist(w),collapse="\n"))
                 })
        } else {
          dsd_xml<-NULL
        }
      } else {
        tryCatch({utils::download.file(dsd_endpoint,temp,dmethod,quiet=TRUE)},
                 error = function(e) {
                 },
                 warning = function(w) {
                 })
        if (file.size(temp)!=0) {
          tryCatch({dsd_xml<-xml2::read_xml(temp)},
                 error = function(e) {
                   dsd_xml<-NULL
                 },
                 warning = function(w) {
                 })
        } else {
          dsd_xml<-NULL
        }
      }
      unlink(temp)
      if (!is.null(dsd_xml)){
        concepts<-xml2::xml_attr(xml2::xml_find_all(dsd_xml,"//str:ConceptIdentity//Ref"),"id")
        if (Sys.info()[['sysname']]=='Windows'){
          dsd_xml<-as.character(dsd_xml)
          cl<-parallel::makeCluster(getOption("restatapi_cores",1L))
          parallel::clusterEvalQ(cl,require(xml2))
          parallel::clusterExport(cl,c("extract_dsd"))
          parallel::clusterExport(cl,c("dsd_xml"),envir=environment())
          dsd<-data.frame(do.call(rbind,parallel::parLapply(cl,concepts,extract_dsd,dsd_xml=dsd_xml)),stringsAsFactors=FALSE)
          parallel::stopCluster(cl)
        }else{
          dsd<-data.frame(do.call(rbind,parallel::mclapply(concepts,extract_dsd,dsd_xml=dsd_xml,mc.cores=getOption("restatapi_cores",1L))),stringsAsFactors=FALSE)
        }  
        names(dsd)<-c("concept","code","name")
        if (cache){
          pl<-put_eurostat_cache(dsd,paste0(id,".dsd"),update_cache,cache_dir,compress_file)
          if (verbose) {message("get_eurostat_dsd - The DSD of the ",id," dataset was cached ",pl,".\n")}
        }  
      } else {
#       dsd<-NULL
        if (verbose) {
          message("get_eurostat_dsd - The dsd_xml is NULL. Please check in a browser the url below. If it provides valid reponse you can try again to download the DSD.\n ",dsd_endpoint)
        }
      }
    }
    if (!is.null(dsd)){
      data.table::as.data.table(dsd,stringsAsFactors=FALSE)
    }
  }
  return(dsd)
}