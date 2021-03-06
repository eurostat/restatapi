#' @title Download and extract compressed SDMX XML 
#' @description Downloads  and extracts the data values from the SDMX XML data file
#' @param url a URL from the bulk download facility to download the zipped SDMX XML file
#' @param verbose a logical value with default \code{FALSE}, so detailed messages (for debugging) will not printed.
#'         Can be set also with \code{options(restatapi_verbose=TRUE)}.        
#' @export 
#' @details It is a sub-function to use in the \code{\link{get_eurostat_raw}} and the \code{\link{get_eurostat_data}} functions.
#' @return an xml class object with SDMX tags extracted and read from the downloaded file.  
#' @examples 
#' base_url<-"https://ec.europa.eu/eurostat/"
#' url_end<-"estat-navtree-portlet-prod/BulkDownloadListing?file=data/agr_r_milkpr.sdmx.zip"
#' url<-paste0(base_url,url_end)
#' sdmx_xml<-get_compressed_sdmx(url,verbose=TRUE)
#' 

get_compressed_sdmx<-function(url=NULL,verbose=FALSE){
  xml<-xml_fajl<-NULL
  tbc<-TRUE # to be continued
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  dmethod<-getOption("restatapi_dmethod",get("dmethod",envir=restatapi::.restatapi_env))
  if (is.null(url)){
    message("The url is missing.")
    return(NULL)
  } else {
    temp<-tempfile()
    if (verbose) {
      tryCatch({utils::download.file(url,temp,dmethod)},
               error = function(e) {
                 message("get_compressed_sdmx - Error by the download of the SDMX file:",'\n',paste(unlist(e),collapse="\n"))
                 tbc<-FALSE
               },
               warning = function(w) {
                 message("get_compressed_sdmx - Warning during the download of the SDMX file:",'\n',paste(unlist(w),collapse="\n"))
                 tbc<-FALSE
               })
    } else {
      tryCatch({utils::download.file(url,temp,dmethod,quiet=TRUE)},
               error = function(e) {tbc<-FALSE},
               warning = function(w) {tbc<-FALSE})
    }
    if (tbc) {
      if (grepl("Bulk",url)){
        fajl<-paste0(sub("\\..*$","",sub("^.*\\/","",url,perl=TRUE),perl=TRUE),".sdmx")
      } else {
        fajl<-paste0("DataResponse-",sub("^.*\\/","",url,perl=TRUE))
      }
      if (verbose) {
        tryCatch({xml_fajl<-utils::unzip(temp,paste0(fajl,".xml"))},
                 error = function(e) {
                   message("get_compressed_sdmx - Error during the unzip of the SDMX file:",'\n',paste(unlist(e),collapse="\n"))
                 },
                 warning = function(w) {
                   message("get_compressed_sdmx - Warning by the unzip of the SDMX file:",'\n',paste(unlist(w),collapse="\n"))
                 })
      } else {
        tryCatch({xml_fajl<-utils::unzip(temp,paste0(fajl,".xml"))},
                 error = function(e) {},
                 warning = function(w) {})
      }
    }  
  }
  if (!is.null(xml_fajl)){xml<-xml2::read_xml(xml_fajl)}
  unlink(temp)
  unlink(paste0(fajl,".xml"))
  return(xml)
}