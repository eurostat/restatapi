#' @title Download and extract compressed SDMX XML 
#' @description Downloads  and extracts the data values from the SDMX XML data file
#' @param url a URL from the bulk download facility to download the zipped SDMX XML file
#' @param verbose a logical value with default \code{FALSE}, so detailed messages (for debugging) will not printed.
#'         Can be set also with \code{options(restatapi_verbose=TRUE)}. 
#' @param format the format of the compression, either "zip" or "gz" the default value       
#' @export 
#' @details It is a sub-function to use in the \code{\link{get_eurostat_raw}} and the \code{\link{get_eurostat_data}} functions.
#' @return an xml class object with SDMX tags extracted and read from the downloaded file.  
#' @examples 
#' if (!(grepl("amzn|-aws|-azure ",Sys.info()['release']))) options(timeout=2)
#' id<-"agr_r_milkpr"
#' url<-paste0("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
#'             id,
#'             "?format=sdmx_2.1_structured&compressed=true")
#' options(timeout=2)
#' sdmx_xml<-get_compressed_sdmx(url,verbose=TRUE,format="gz")
#' options(timeout=60)

get_compressed_sdmx<-function(url=NULL,verbose=FALSE,format="gz"){
  xml<-xml_fajl<-NULL
  tbc<-TRUE # to be continued
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  dmethod<-getOption("restatapi_dmethod",get("dmethod",envir=restatapi::.restatapi_env))
  if (is.null(url)){
    message("The url is missing.")
    return(NULL)
  } else {
    if (tbc) {
      if (verbose) message("\nget_compressed_sdmx - url:",url,"\nget_compressed_sdmx - format:",format)

      if (format=="zip"){ # zip format
        # download file 
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
                   error = function(e) {
                     message("There is an error by the download of the SDMX file. Run the same command with verbose=TRUE option to get more info on the issue.")
                     tbc<-FALSE},
                   warning = function(w) {
                     message("There is a warning by the download of the SDMX file. Run the same command with verbose=TRUE option to get more info on the issue.")
                     tbc<-FALSE})
        }
        
        # unzip file
        if (grepl("Bulk",url)){
          fajl<-paste0(sub("\\..*$","",sub("^.*\\/","",url,perl=TRUE),perl=TRUE),".sdmx")
        } else {
          fajl<-paste0("DataResponse-",sub("^.*\\/","",url,perl=TRUE))
        }
        
        tmpdir<-tempdir()
        if (verbose) {
          tryCatch({xml_fajl<-utils::unzip(temp,paste0(fajl,".xml"),exdir=tmpdir)},
                   error = function(e) {
                     message("get_compressed_sdmx - Error during the unzip of the SDMX file:",'\n',paste(unlist(e),collapse="\n"))
                   },
                   warning = function(w) {
                     message("get_compressed_sdmx - Warning by the unzip of the SDMX file:",'\n',paste(unlist(w),collapse="\n"))
                   })
        } else {
          tryCatch({xml_fajl<-utils::unzip(temp,paste0(fajl,".xml"),exdir=tmpdir)},
                   error = function(e) { message("There is an error by the unzipping of the downloaded SDMX file. Run the same command with verbose=TRUE option to get more info on the issue.")},
                   warning = function(w) { message("There is an warning by the unzipping of the downloaded SDMX file. Run the same command with verbose=TRUE option to get more info on the issue.")})
        }
        if (!is.null(xml_fajl)){xml<-xml2::read_xml(xml_fajl)} else {xml<-NULL}
          unlink(temp)
          unlink(file.path(tmpdir,paste0(fajl,".xml")))
        } else if (format=="gz"){
        if (verbose) {
          tryCatch({
                 cid<-gzcon(url(url,open="rb"))
                 xml<-xml2::read_xml(cid)
                 close(cid)},
                 error = function(e) {
                   message("get_compressed_sdmx - Error during retrieval and extraction of the SDMX file:",'\n',paste(unlist(e),collapse="\n"))
                 },
                 warning = function(w) {
                   message("get_compressed_sdmx - Warning by the retrieval and extraction of the SDMX file:",'\n',paste(unlist(w),collapse="\n"))
                 })
        } else {
          tryCatch({
                    cid<-gzcon(url(url,open="rb"))
                    xml<-xml2::read_xml(cid)
                    close(cid)},
                    error = function(e) { message("There is an error by the download of the SDMX file. Run the same command with verbose=TRUE option to get more info on the issue.")},
                    warning = function(w) { message("There is an warning by the download of the SDMX file. Run the same command with verbose=TRUE option to get more info on the issue.")})
        }
        if (verbose) message("get_compressed_sdmx - xml NULL:",is.null(xml))  
        # if (!is.null(xml_fajl)){xml<-xml2::read_xml(gzcon(url(url,open="rb")))} else {xml<-NULL}
        

      } else {
        message("Incorrect compression format. The compression format should be either 'gz' or 'zip'.") 
      }
    }  
  }
  return(xml)
}