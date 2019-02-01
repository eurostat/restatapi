#' @title Get Eurostat data in a standardized format
#' @description Download data sets from \href{https://ec.europa.eu/eurostat/}{Eurostat} database and put in a standardized format.
#' @param id A code name for the dataset of interest.
#'        See \code{\link{search_eurostat_toc}} for details how to get an id.
#' @param select_freq a character symbol for a time frequency when a dataset has multiple time
#'        frequencies. Possible values are:
#'    	  A = annual, S = semi-annual, Q = quarterly, M = monthly. 
#'    	  The default is \code{NULL} as most datasets have just one time
#'        frequency and in this case if there are multiple frequencies, then only the most common frequency kept.
#'        If all the frequencies needed the \code{\link{get_eurostat_raw}} can be used.
#' @param cache a logical whether to do caching. Default is \code{TRUE}.
#' @param update_cache a logical with a default value \code{FALSE}, whether to update cache. Can be set also with
#'        \code{options(restatapi_update = TRUE)}
#' @param cache_dir a path to a cache directory. The \code{NULL} (default) uses the memory as cache. 
#'        If the folder  if the \code{cache_dir} directory does not exist it saves in the 'restatapi' directory 
#'        under the temporary directory from \code{tempdir()}. Directory can also be set with
#'        \code{option(restatapi_cache_dir=...)}.
#' @param compress_file a logical whether to compress the
#'        RDS-file in caching. Default is \code{TRUE}.
#' @param stringsAsFactors if \code{TRUE} (the default) variables are not numeric then they are
#'        converted to factors. If the value \code{FALSE}
#'        they are returned as a characters.
#' @param keep_flags a logical whether the observation status (flags) - e.g. "confidential",
#'        "provisional", etc. - should be kept in a separate column or if they
#'        can be removed. Default is \code{FALSE}. For flag values see: 
#'        \url{http://ec.europa.eu/eurostat/data/database/information}.
#' @param verbose A boolean with default \code{FALSE}, so detailed messages (for debugging) will not printed.
#'         Can be set also with \code{options(restatapi_verbose=TRUE)}
#' @param ... parameter to pass on the \code{\link{load_cfg}} function        
#' @export
#' 
#' @details Data sets are downloaded from \href{http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing}{the Eurostat bulk download facility} 
#' in SDMX format and filtered for a unique time frequency.
#' If no frequency is selected and there are multiple frequencies in the dataset, then the most common value is used.
#' 
#' Compared to \code{\link{get_eurostat_raw}} the frequency column is removed  
#' and the original column names for the time period, observation values and status are renamed to "time", "values" and "flags".
#' 
#' By default all datasets cached as they are often rather large. 
#' The datasets cached in memory (default) or can be stored in a temporary directory if \code{cache_dir} or \code{option(restatpi_cache_dir)} is defined.
#' The cache can be emptied with \code{\link{clean_restatapi_cache}}.
#' 
#' The \code{id}, is a value from the \code{code} column of the table of contents (\code{\link{get_eurostat_toc}}), and can be searched for with the \code{\link{search_eurostat_toc}} function. The id value can be retrieved from the \href{http://ec.europa.eu/eurostat/data/database}{Eurostat database}
#'  as well. The Eurostat
#' database gives codes in the Data Navigation Tree after every dataset
#' in parenthesis.
#' @return a data.table. One column for each dimension in the data,
#'         the time column for a time dimension, 
#'         the values column for numerical values and the flags column if the \code{keep_flags=TRUE}.
#'         Eurostat data does not include all missing values. The missing values are dropped if all dimensions are missing
#'         on particular time. 
#' @seealso \code{\link{get_eurostat_data}}, \code{\link{get_eurostat_raw}}
#' @examples 
#' \dontrun{
#' dt<-get_eurostat_bulk("tps00201")
#' dt<-get_eurostat_bulk("agr_r_milkpr",keep_flags=T)
#' dt<-get_eurostat_bulk("avia_par_ee")
#' dt<-get_eurostat_bulk("avia_par_ee",select_freq="A")
#' dt<-get_eurostat_bulk("avia_par_ee",update_cache=T)
#' options(restatapi_update=T)
#' dt<-get_eurostat_bulk("tps00201",cache_dir="/tmp",compress_file=F)
#' }

get_eurostat_bulk <- function(id,
                              cache=TRUE,
                              update_cache=FALSE,
                              cache_dir=NULL,
                              compress_file=TRUE,
                              stringsAsFactors=default.stringsAsFactors(),
                              select_freq=NULL,
                              keep_flags = FALSE,
                              verbose=FALSE,...){
  
  .datatable.aware=T 
  FREQ<-N<-restat<-NULL
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  update_cache<-update_cache|getOption("restatapi_update", FALSE)
  if (!(exists(".restatapi_env"))) {load_cfg(...)}
  cfg<-get("cfg",envir=.restatapi_env) 
  rav<-get("rav",envir=.restatapi_env)
  id<-tolower(id)
  
  toc<-get_eurostat_toc()
  if ((cache)&(!update_cache)) {
    udate<-toc$lastUpdate[toc$code==id]
    restat<-get_eurostat_cache(paste0(id,"_", udate,"_",sum(keep_flags),sub("_$","",paste0("_",select_freq),perl=T)),cache_dir)
    if ((!is.null(restat))&(verbose)) {message("The data was loaded from cache.")}  
  }
  if ((!cache)|(is.null(restat))|(update_cache)){
    restat<-get_eurostat_raw(id,cache,update_cache,cache_dir,compress_file,stringsAsFactors,keep_flags,...)
  }  
  drop=c("FREQ","TIME_FORMAT")
  if ((is.null(select_freq))){
    if (length(unique(restat$FREQ))>1){
      st<-data.table::setorder(restat[,.N,by=FREQ],-N)[1,1]
      if (stringsAsFactors){select_freq<-as.character(levels(st$FREQ)[st$FREQ[1]])}else{as.character(st$FREQ)}
      warning("There are multiple frequencies in the dataset. The '", select_freq, "' is selected as it is the most common frequency.")
    } 
  }
  if (!(is.null(select_freq))){restat<-restat[FREQ==select_freq]}
  if ("OBS_VALUE" %in% colnames(restat)) {
    if (keep_flags){
      data.table::setnames(restat,"OBS_STATUS","flags")
    } else {
     if ("OBS_STATUS" %in% colnames(restat)) {drop<-c(drop,"OBS_STATUS")}    
    }
    restat[,(drop):=NULL]
    data.table::setnames(restat,c("TIME_PERIOD","OBS_VALUE"),c("time","values"))
  }
  if (is.factor(restat$values)){restat$values<-as.numeric(levels(restat$values))[restat$values]} else{restat$values<-as.numeric(restat$values)}
  if (cache){
    udate<-toc$lastUpdate[toc$code==id]
    pl<-put_eurostat_cache(restat,paste0(id,"-",udate,"-",sum(keep_flags),sub("-$","",paste0("-",select_freq),perl=T)),cache_dir,compress_file)
    if ((!is.null(pl))&(verbose)) {message("The data was cached ",pl,".\n" )}
  }
  restat
}
