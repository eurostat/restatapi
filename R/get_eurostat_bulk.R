#' @title Get Eurostat data in a standardized format
#' @description Download data sets from \href{https://ec.europa.eu/eurostat/}{Eurostat} database and put in a standardized format.
#' @param id A code name for the dataset of interest.
#'        See \code{\link{search_eurostat_toc}} for details how to get an id.
#' @param cache a logical whether to do caching. Default is \code{TRUE}.
#' @param update_cache a logical with a default value \code{FALSE}, whether to update cache. Can be set also with
#'        \code{options(restatapi_update=TRUE)}
#' @param cache_dir a path to a cache directory. The \code{NULL} (default) uses the memory as cache. 
#'        If the folder  if the \code{cache_dir} directory does not exist it saves in the 'restatapi' directory 
#'        under the temporary directory from \code{tempdir()}. Directory can also be set with
#'        \code{option(restatapi_cache_dir=...)}.
#' @param compress_file a logical whether to compress the
#'        RDS-file in caching. Default is \code{TRUE}.
#' @param stringsAsFactors if \code{TRUE} (the default) variables are not numeric then they are
#'        converted to factors. If the value \code{FALSE}
#'        they are returned as a characters.
#' @param select_freq a character symbol for a time frequency when a dataset has multiple time
#'        frequencies. Possible values are:
#'    	  A = annual, S = semi-annual, H = half-year, Q = quarterly, M = monthly, W = weekly, D = daily. 
#'    	  The default is \code{NULL} as most datasets have just one time frequency. 
#'    	  In case if there are multiple frequencies and \code{select_freq=NULL}, then only the most common frequency kept.
#'        If all the frequencies needed the \code{\link{get_eurostat_raw}} can be used.
#' @param keep_flags a logical whether the observation status (flags) - e.g. "confidential",
#'        "provisional", etc. - should be kept in a separate column or if they
#'        can be removed. Default is \code{FALSE}. For flag values see: 
#'        \url{http://ec.europa.eu/eurostat/data/database/information}.
#' @param cflags a logical whether the missing observations with flag 'c' - "confidential"
#'        should be kept or not. Default is \code{FALSE}, in this case these observations dropped from the dataset. If this parameter 
#'        \code{TRUE} then the flags are kept and the parameter provided in \code{keep_flags} is not taken into account.  
#' @param check_toc a boolean whether to check the provided \code{id} in the Table of Contents (TOC) or not. The default value 
#'        \code{FALSE}, in this case the base URL for the download link is retrieved from the configuration file. 
#'        If the value is \code{TRUE} then the TOC is downloaded and the \code{id} is checked in it. If it found then the download link 
#'        is retrieved form the TOC.  
#' @param verbose A boolean with default \code{FALSE}, so detailed messages (for debugging) will not printed.
#'         Can be set also with \code{options(restatapi_verbose=TRUE)}
#' @param ... other parameter(s) to pass on the \code{\link{load_cfg}} function        
#' @export
#' 
#' @details Data sets are downloaded from \href{http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing}{the Eurostat bulk download facility} 
#' in TSV format as in this case smaller file has to be downloaded and processed. If there is more then one frequency then
#'  the is filtered for a unique time frequency.
#' If no frequency is selected and there are multiple frequencies in the dataset, then the most common value is used used for frequency.
#' 
#' Compared to \code{\link{get_eurostat_raw}} the frequency (FREQ) and time format (TIME_FORMAT) columns are not included  
#' and the column names for the time period, observation values and status have standardised names: "time", "values" and "flags" 
#' independently if the data was downloaded previously through SDMX or TSV format.
#' 
#' By default all datasets cached as they are often rather large. 
#' The datasets cached in memory (default) or can be stored in a temporary directory if \code{cache_dir} or \code{option(restatpi_cache_dir)} is defined.
#' The cache can be emptied with \code{\link{clean_restatapi_cache}}.
#' 
#' The \code{id}, is a value from the \code{code} column of the table of contents (\code{\link{get_eurostat_toc}}), and can be searched for with the \code{\link{search_eurostat_toc}} function. The id value can be retrieved from the \href{http://ec.europa.eu/eurostat/data/database}{Eurostat database}
#'  as well. The Eurostat database gives codes in the Data Navigation Tree after every dataset
#' in parenthesis.
#' @return a data.table with the following columns: #'  \tabular{ll}{
#'      dimension names \tab One column for each dimension in the data \cr
#'      \code{time} \tab A column for the time dimension\cr
#'      \code{values} \tab A column for numerical values\cr
#'      \code{flags} \tab A column for flags if the \code{keep_flags=TRUE} otherwise this column is not included in the data table
#'    }
#'   The data.table does not include all missing values. The missing values are dropped if the value and flag are missing
#'         on a particular time.
#' @seealso \code{\link{get_eurostat_data}}, \code{\link{get_eurostat_raw}}
#' @examples 
#' \dontshow{
#' if (parallel::detectCores()<=2){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }    
#' }
#' \donttest{
#' dt<-get_eurostat_bulk("agr_r_milkpr",keep_flags=TRUE)
#' options(restatapi_update=TRUE)
#' dt<-get_eurostat_bulk("avia_par_ee",check_toc=TRUE)
#' dt<-get_eurostat_bulk("avia_par_ee",select_freq="A",verbose=TRUE)
#' options(restatapi_update=FALSE)
#' dt<-get_eurostat_bulk("agr_r_milkpr",cache_dir=tempdir(),compress_file=FALSE,verbose=TRUE)
#' }

get_eurostat_bulk <- function(id,
                              cache=TRUE,
                              update_cache=FALSE,
                              cache_dir=NULL,
                              compress_file=TRUE,
                              stringsAsFactors=default.stringsAsFactors(),
                              select_freq=NULL,
                              keep_flags=FALSE,
                              cflags=FALSE,
                              check_toc=FALSE,
                              verbose=FALSE,...){
  
  .datatable.aware=TRUE 
  FREQ<-N<-restat_bulk<-NULL
  dc<-TRUE
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  update_cache<-update_cache|getOption("restatapi_update", FALSE)
  if(cflags){keep_flags<-cflags}
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
  id<-tolower(id)
  
  if (check_toc){
    toc<-get_eurostat_toc(verbose=verbose)
    if (is.null(toc)){
      message("The TOC is missing. Could not get the download link.")
      dc<-FALSE
    } else {
      if (any(grepl(id,toc$code,ignore.case=TRUE))){
        udate<-toc$lastUpdate[grepl(id,toc$code,ignore.case=TRUE)]
        if (verbose) {message("bulk TOC rows: ",nrow(toc),"\nbulk url: ",toc$downloadLink.tsv[grepl(id,toc$code,ignore.case=TRUE)],"\ndata rowcount: ",toc$values[grepl(id,toc$code,ignore.case=TRUE)])}
      } else {
        message(paste0("'",id,"' is not in the table of contents. Please check if the 'id' is correctly spelled."))
        dc<-FALSE
      }
    }
  }else{
    udate<-format(Sys.Date(),"%Y.%m.%d")
  }
  
  if (dc) {
    if ((cache)&(!update_cache)) {
      nm<-paste0("b_",id,"-",udate,"-",sum(keep_flags),"-",sum(cflags),sub("-$","",paste0("-",select_freq),perl=TRUE))
      restat_bulk<-data.table::copy(get_eurostat_cache(nm,cache_dir,verbose=verbose))
    }

    if ((!cache)|is.null(restat_bulk)|(update_cache)){
      restat_bulk<-get_eurostat_raw(id,"txt",cache,update_cache,cache_dir,compress_file,stringsAsFactors,keep_flags,check_toc,verbose=verbose)
    }
  }  
  
  if (!is.null(restat_bulk)){
    restat_bulk[]
    drop<-NULL
    if ("FREQ" %in% colnames(restat_bulk)) {drop=c("FREQ")}
    if ("TIME_FORMAT" %in% colnames(restat_bulk)) {drop<-c(drop,"TIME_FORMAT")} 
    if (is.null(select_freq)){
      if (length(unique(restat_bulk$FREQ))>1){
        st<-data.table::setorder(restat_bulk[,.N,by=FREQ],-N)[1,1]
        if (is.factor(st$FREQ)){select_freq<-as.character(levels(st$FREQ)[st$FREQ[1]])}else{select_freq<-as.character(st$FREQ)}
          message("There are multiple frequencies in the dataset. The '", select_freq, "' is selected as it is the most common frequency.")
        } 
      } 
    if (!(is.null(select_freq))){restat_bulk<-restat_bulk[restat_bulk$FREQ==select_freq,][]}
    if ("OBS_VALUE" %in% colnames(restat_bulk)) {
      if (keep_flags){
        data.table::setnames(restat_bulk,"OBS_STATUS","flags")[]
      } else {
        if ("OBS_STATUS" %in% colnames(restat_bulk)) {drop<-c(drop,"OBS_STATUS")}    
      }
      data.table::setnames(restat_bulk,c("TIME_PERIOD","OBS_VALUE"),c("time","values"))[]
    }
    restat_bulk$time<-gsub('[MD]',"-",restat_bulk$time)
    restat_bulk$time<-gsub('([0-9]{4})([Q|S])',"\\1-\\2",restat_bulk$time,perl=TRUE)
    if (keep_flags) {
      restat_bulk$flags<-as.character(restat_bulk$flags)
      restat_bulk$flags[is.na(restat_bulk$flags)]<-""
      if (!cflags) {restat_bulk<-restat_bulk[restat_bulk$flags!="c"][]}
    } else if ("flags" %in% colnames(restat_bulk)){
      drop<-c(drop,"flags")
    }
    if(!is.null(drop)) {restat_bulk[,(drop):=NULL][]}
    if (any(sapply(restat_bulk,is.factor))&(!stringsAsFactors)) {
      col_conv<-colnames(restat_bulk)[!(colnames(restat_bulk) %in% c("values"))]
      restat_bulk[,col_conv]<-restat_bulk[,lapply(.SD,as.character),.SDcols=col_conv][]
    }
    if (any(!sapply(restat_bulk[],is.factor))&(stringsAsFactors)) {
      restat_bulk<-data.table::data.table(restat_bulk,stringsAsFactors=stringsAsFactors)[]
    }  
    if (is.factor(restat_bulk$values)){restat_bulk$values<-as.numeric(levels(restat_bulk$values))[restat_bulk$values]} else{restat_bulk$values<-as.numeric(restat_bulk$values)}
  } 
   
  if ((!is.null(restat_bulk))&cache&(all(!grepl("get_eurostat_data",as.character(sys.calls()),perl=TRUE)))){
    oname<-paste0("b_",id,"-",udate,"-",sum(keep_flags),"-",sum(cflags),sub("-$","",paste0("-",select_freq),perl=TRUE))
    pl<-put_eurostat_cache(restat_bulk,oname,update_cache,cache_dir,compress_file)
    if ((!is.null(pl))&(verbose)) {message("The bulk data was cached ",pl,".\n" )}
  }
  
  return(restat_bulk)
}
