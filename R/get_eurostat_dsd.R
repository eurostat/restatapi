#' @title Download the Data Structure Definition of a dataset
#' @description Download Data Structure Definition (DSD) of a Eurostat dataset if it is not cached previously. 
#' @param id a character string with the id of the dataset. It is the value from the codename column of the \code{get_eurostat_toc} function. 
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
#' if ((parallel::detectCores()<2)|(Sys.info()[['sysname']]=='Windows')){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }
#' }
#' dsd<-get_eurostat_dsd("nama_10_gdp",cache=FALSE)
#' head(dsd)
#' 

get_eurostat_dsd <- function(id,
                             cache=TRUE,
                             update_cache=FALSE,
                             cache_dir=NULL,
                             compress_file=TRUE,
                             verbose=FALSE,...) {
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  if (is.null(id)){
    stop('No dataset id were provided.')
  } else {
    dsd<-NULL
    if (!(exists(".restatapi_env"))) {load_cfg(...)}
    update_cache <- update_cache | getOption("restatapi_update", FALSE)
    if ((cache) & (!update_cache)) {
      dsd<-get_eurostat_cache(paste0(id,".dsd"),cache_dir,verbose=verbose)
    }
    if ((!cache)|(is.null(dsd))|(update_cache)){
      cfg<-get("cfg",envir=.restatapi_env) 
      rav<-get("rav",envir=.restatapi_env)
      dsd_endpoint <- paste0(eval(parse(text=paste0("cfg$QUERY_BASE_URL$'",rav,"'$ESTAT$data$'2.1'$datastructure"))),"/DSD_",id)
      if (verbose) {
        message(dsd_endpoint)
        tryCatch({dsd_xml<-xml2::read_xml(dsd_endpoint)},
                 error = function(e) {
                   message("Unable to download the DSD:",'\n',paste(unlist(e),collapse="\n"))
                 },
                 warning = function(w) {
                   message("Unable to download the DSD:",'\n',paste(unlist(w),collapse="\n"))
                 })
      } else {
        tryCatch({dsd_xml<-xml2::read_xml(dsd_endpoint)},
                 error = function(e) {
                 },
                 warning = function(w) {
                 })
      }
      if (exists("dsd_xml")){
        concepts<-xml2::xml_attr(xml2::xml_find_all(dsd_xml,"//str:ConceptIdentity//Ref"),"id")
        dsd<-data.frame(do.call(rbind,parallel::mclapply(concepts,extract_dsd,dsd_xml=dsd_xml,mc.cores=getOption("restatapi_cores",1L))),stringsAsFactors=FALSE)
        names(dsd)<-c("concept","code","name")
        if (cache){
          pl<-put_eurostat_cache(dsd,paste0(id,".dsd"),update_cache,cache_dir,compress_file)
          if (verbose) {message("The DSD of the ",id," dataset was cached ",pl,".\n")}
        } 
      } else {
        dsd<-NULL
      }
    }
    if (!is.null(dsd)){
      data.table::as.data.table(dsd)
    } else {
      NULL
    }
  }    
}