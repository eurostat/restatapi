#' @title Extract data values from SDMX XML 
#' @description Extracts the data values from the SDMX XML data file
#' @param xml_lf an XML leaf with data series from an SDMX XML file
#' @param keep_flags a boolean if to extract the observation status (flag) information from the XML file. The default value is \code{FALSE}
#' @param stringsAsFactors if \code{TRUE} (the default) the columns are
#'        converted to factors. If \code{FALSE} they are returned as a character.
#' @export 
#' @details It is a subfunction to use in the \code{\link{get_eurostat_data}} function.
#' @return a data frame containing the values of the SDMX files
#' @examples 
#' \dontshow{
#' if (parallel::detectCores()<=2){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }
#' }
#' \donttest{
#' id<-"agr_r_milkpr"
#' toc<-get_eurostat_toc()
#' bulk_url<-toc$downloadLink.sdmx[toc$code==id]
#' temp<-tempfile()
#' download.file(bulk_url,temp)
#' sdmx_xml<-xml2::read_xml(unzip(temp, paste0(id,".sdmx.xml")))
#' xml_leafs<-xml2::xml_find_all(sdmx_xml,".//data:Series")
#' extract_data(xml_leafs[1])
#' }
#' 

extract_data<-function(xml_lf,keep_flags=FALSE,stringsAsFactors=default.stringsAsFactors()){
  if (keep_flags){
    cn<-c("TIME_PERIOD","OBS_VALUE","OBS_STATUS")
  } else {
    cn<-c("TIME_PERIOD","OBS_VALUE")
  }
  if (Sys.info()[['sysname']]=='Windows'){xml_lf<-xml2::as_xml_document(xml_lf)}
  bd<-t(as.data.frame(xml2::xml_attrs(xml_lf)))
  rownames(bd)<-NULL
  dv<-xml2::xml_attrs(xml2::xml_children(xml_lf))
  df<-do.call(rbind, lapply(lapply(dv, unlist),"[", cn))
  colnames(df)<-cn
  data.frame(bd,df,stringsAsFactors=stringsAsFactors)
}