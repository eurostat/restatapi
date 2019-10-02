#' @title Get Eurostat data as it is 
#' @description Download data sets from \href{https://ec.europa.eu/eurostat}{Eurostat} database .
#' @param id A code name for the dataset of interest.
#'        See \code{\link{search_eurostat_toc}} for details how to get an id.
#' @param mode defines the format of the downloaded dataset. It can be \code{txt} (the default value) for TSV 
#'        (Tab Separated Values), or \code{xml} for the SDMX version. 
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
#' \dontshow{
#' if (parallel::detectCores()<=2){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }    
#' }
#' \donttest{
#' dt<-get_eurostat_raw("agr_r_milkpr",keep_flags=TRUE)
#' dt<-get_eurostat_raw("avia_par_ee",mode="xml",update_cache=TRUE)
#' options(restatapi_update=FALSE)
#' dt<-get_eurostat_raw("avia_par_me",mode="txt",cache_dir=tempdir(),compress_file=FALSE,verbose=TRUE)
#' }

get_eurostat_raw <- function(id, 
                             mode="txt",
                             cache=TRUE, 
                             update_cache=FALSE,
                             cache_dir=NULL,
                             compress_file=TRUE,
                             stringsAsFactors=default.stringsAsFactors(),
                             keep_flags=FALSE,
                             verbose=FALSE,...){
  
  restat_raw<-NULL
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  update_cache<-update_cache|getOption("restatapi_update", FALSE)
  ne<-ne2<-ne3<-TRUE
  if (!(exists(".restatapi_env"))) {load_cfg(...)}
  cfg<-get("cfg",envir=.restatapi_env) 
  rav<-get("rav",envir=.restatapi_env)
  id<-tolower(id)
  
  toc<-get_eurostat_toc(verbose=verbose)
  if (is.null(toc)){
    message("The TOC is missing. Could not get the download link.")
  } else {
    if ((cache)&(!update_cache)) {
      udate<-toc$lastUpdate[toc$code==id]
      restat_raw<-get_eurostat_cache(paste0("r_",id,"-",udate,"-",sum(keep_flags)),cache_dir,verbose=verbose)
      if (!is.null(restat_raw)){
        if (any(sapply(restat_raw,is.factor))&(!stringsAsFactors)) {
          col_conv<-colnames(restat_raw)
          restat_raw[,col_conv]<-restat_raw[,lapply(.SD,as.character),.SDcols=col_conv]
        }
        if (!any(sapply(restat_raw,is.factor))&(stringsAsFactors)&(!is.null(restat_raw))) {
          restat_raw<-data.table::data.table(restat_raw,stringsAsFactors=TRUE)
        }
        if ((!keep_flags) & ("OBS_STATUS" %in% colnames(restat_raw)))  {restat_raw$OBS_STATUS<-NULL}
      }
    }
    if ((!cache)|(is.null(restat_raw))|(update_cache)){
      if (mode=="txt") {
        bulk_url<-toc$downloadLink.tsv[toc$code==id]
      } else if (mode=="xml") {
        bulk_url<-toc$downloadLink.sdmx[toc$code==id]
      } else {
        bulk_url<-NULL
        message("Incorrect mode:",mode,"\n It should be either 'txt' or 'xml'." )
      }
      if (!is.null(bulk_url)){
        if (length(bulk_url)!=0){
          if (mode=="txt"){
            temp <- tempfile()
            if (verbose){
              message("TOC rows: ",nrow(toc),"\nbulk url: ",bulk_url,"\ndata rowcount: ",toc$values[toc$code==id])
              tryCatch({utils::download.file(bulk_url,temp)},
                       error = function(e) {
                         message("Unable to download the TSV file:",'\n',paste(unlist(e),collapse="\n"))
                         ne<-FALSE
                       },
                       warning = function(w) {
                         message("Unable to download the TSV file:",'\n',paste(unlist(w),collapse="\n"))
                       })
            } else {
              tryCatch({utils::download.file(bulk_url,temp)},
                       error = function(e) {ne<-FALSE},
                       warning = function(w) {})
            }
            if (ne){
              if (verbose){
                message("TOC rows: ",nrow(toc),"\nbulk url: ",bulk_url,"\ndata rowcount: ",toc$values[toc$code==id])
                tryCatch({gz<-gzfile(temp, open = "rt")
                raw<-data.table::fread(text=readLines(gz),sep='\t',sep2=',',colClasses='character',header=TRUE)
                close(gz)
                unlink(temp)},
                error = function(e) {
                  message("Unable to open the downloaded TSV file:",'\n',paste(unlist(e),collapse="\n"))
                  ne2<-FALSE
                },
                warning = function(w) {
                  message("Unable to open the downloaded TSV file:",'\n',paste(unlist(w),collapse="\n"))
                })
              } else {
                tryCatch({gz<-gzfile(temp, open = "rt")
                raw<-data.table::fread(text=readLines(gz),sep='\t',sep2=',',colClasses='character',header=TRUE)
                close(gz)
                unlink(temp)},
                error = function(e) {ne2<-FALSE},
                warning = function(w) {})
              }
              if (ne2) {
                cname<-colnames(raw)[1] 
                if (is.character(cname)){
                  cnames<-utils::head(unlist(strsplit(cname,(',|\\\\'))),-1)
                  rname<-utils::tail(unlist(strsplit(cname,(',|\\\\'))),1)
                  data.table::setnames(raw,1,"bdown")
                  raw_melted<-data.table::melt.data.table(raw,"bdown",variable.factor=stringsAsFactors)
                  rm(raw)
                  data.table::setnames(raw_melted,2:3,c(rname,"values"))
                  raw_melted<-raw_melted[raw_melted$values!=":",]
                  restat_raw<-data.table::as.data.table(data.table::tstrsplit(raw_melted$bdown,",",fixed=TRUE))
                  data.table::setnames(restat_raw,cnames)  
                  restat_raw<-data.table::data.table(restat_raw,raw_melted[,2:3])
                  if (keep_flags) {restat_raw$flags<-gsub('[0-9\\.-]',"",restat_raw$values)}
                  restat_raw$values<-gsub('[^0-9\\.-]',"",restat_raw$values)
                  restat_raw<-data.table(restat_raw,stringsAsFactors=stringsAsFactors)  
                } else {
                  message("The file download was not successful. Try again later.")
                  restat_raw<-NULL
                }
              }  
            }
          } else if (mode=="xml"){
            if (verbose) {
              message("TOC rows: ",nrow(toc),"\nbulk url: ",bulk_url,"\ndata rowcount: ",toc$values[toc$code==id])
            }
            sdmx_file<-get_compressed_sdmx(bulk_url,verbose=verbose)
            if(!is.null(sdmx_file)){
              xml_leafs<-xml2::xml_find_all(sdmx_file,".//data:Series")
              if (Sys.info()[['sysname']]=='Windows'){
                xml_leafs<-as.character(xml_leafs)
                cl<-parallel::makeCluster(getOption("restatapi_cores",1L))
                parallel::clusterEvalQ(cl,require(xml2))
                parallel::clusterExport(cl,c("extract_data"))
                restat_raw<-data.table::rbindlist(parallel::parLapply(cl,xml_leafs,extract_data,keep_flags=keep_flags,stringsAsFactors=stringsAsFactors))              
                parallel::stopCluster(cl)
              }else{
                restat_raw<-data.table::rbindlist(parallel::mclapply(xml_leafs,extract_data,keep_flags=keep_flags,stringsAsFactors=stringsAsFactors,mc.cores=getOption("restatapi_cores",1L)))                                  
              }
            }
          }
        }else{
          message("The download link from the TOC is missing.")
          restat_raw<-NULL
          ne3<-FALSE
        }
      }
    }  
    if (ne&ne2&ne3&cache&all(!grepl("get_eurostat_bulk|get_eurostat_data",as.character(sys.calls()),perl=TRUE))){
      oname<-paste0("r_",id,"-",toc$lastUpdate[toc$code==id],"-",sum(keep_flags))
      pl<-put_eurostat_cache(restat_raw,oname,update_cache,cache_dir,compress_file)
      if ((!is.null(pl))&(verbose)) {message("The raw data was cached ",pl,".\n" )}
    }  
  }
  return(restat_raw)
}
