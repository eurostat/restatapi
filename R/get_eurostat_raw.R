#' @title Get Eurostat data as it is 
#' @description Download data sets from \href{https://ec.europa.eu/eurostat}{Eurostat} database .
#' @param id A code name for the dataset of interest.
#'        See \code{\link{search_eurostat_toc}} for details how to get an id.
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
#' @param ... further argument for the \code{load_cfg} function
#' @export
#' 
#' @details Data sets are downloaded from \href{http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing}{the Eurostat bulk download facility} 
#' in SDMX format.
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
#' @seealso \code{\link{get_eurostat_data}}, \code{\link{get_eurostat_bulk}}
#' @examples 
#' \dontrun{
#' dt<-get_eurostat_bulk("tps00201")
#' dt<-get_eurostat_bulk("agr_r_milkpr",keep_flags=T)
#' dt<-get_eurostat_bulk("avia_par_ee",update_cache=T)
#' options(restatapi_update=T)
#' dt<-get_eurostat_bulk("tps00201",cache_dir="/tmp",compress_file=F)
#' }

get_eurostat_raw <- function(id, 
                             cache=TRUE, 
                             update_cache=FALSE,
                             cache_dir=NULL,
                             compress_file=TRUE,
                             stringsAsFactors=default.stringsAsFactors(),
                             keep_flags=FALSE,
                             verbose=FALSE,...){
  
  restat<-NULL
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  update_cache<-update_cache|getOption("restatapi_update", FALSE)
  if (!(exists(".restatapi_env"))) {load_cfg(...)}
  cfg<-get("cfg",envir=.restatapi_env) 
  rav<-get("rav",envir=.restatapi_env)
  id<-tolower(id)
  
  toc<-get_eurostat_toc()
  if ((cache)&(!update_cache)) {
    udate<-toc$lastUpdate[toc$code==id]
    restat<-get_eurostat_cache(paste0(id,"-",udate,"-",sum(keep_flags)),cache_dir)
    if ((!keep_flags) & ("OBS_STATUS" %in% colnames(restat)))  {restat$OBS_STATUS<-NULL}
    if ((!is.null(restat))&(verbose)) {message("The data was loaded from cache.")}  
  }
  if ((!cache)|(is.null(restat))|(update_cache)){
    bulk_url<-toc$downloadLink.sdmx[toc$code==id]
    temp <- tempfile()
    utils::download.file(bulk_url,temp)
    xml_leafs<-xml2::xml_find_all(xml2::read_xml(utils::unzip(temp, paste0(id,".sdmx.xml"))),".//data:Series")
    restat<-data.table::rbindlist(parallel::mclapply(xml_leafs,extract_data,keep_flags=keep_flags,stringsAsFactors=stringsAsFactors))
    unlink(temp)
    unlink(paste0(id,".sdmx.xml"))
  }
  if (cache){
    udate<-toc$lastUpdate[toc$code==id]
    pl<-put_eurostat_cache(restat,paste0(id,"-",udate,"-",sum(keep_flags)),cache_dir,compress_file)
    if ((!is.null(pl))&(verbose)) {message("The data was cached ",pl,".\n" )}
  }
  restat
}
