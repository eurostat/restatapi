#' @title Extract the text of the table of contents from SDMX XML 
#' @description Extracts the values of a node from the Eurostat XML Table of contents (TOC) file
#' @param ns an XML node set from the XML TOC file 
#' @export
#' @details It is a sub-function to use in the \code{\link{get_eurostat_toc}} function.
#' @return a character vector with all the values of the node set.
#' @examples 
#' \dontshow{
#' if (parallel::detectCores()<=2){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }    
#' }
#' cfg<-get("cfg",envir=.restatapi_env) 
#' rav<-get("rav",envir=.restatapi_env)
#' toc_endpoint<-eval(parse(text=paste0("cfg$TOC_ENDPOINT$'",rav,"'$ESTAT$xml")))
#' \donttest{
#' xml_leafs<-xml2::xml_find_all(xml2::read_xml(toc_endpoint),".//nt:leaf")
#' extract_toc(xml_leafs[1])
#' }
#' 

extract_toc<-function(ns){
  if (Sys.info()[['sysname']]=='Windows'){ns<-xml2::as_xml_document(ns)}
  sub("<.*","",sub(".*?>","",as.character(xml2::xml_children(ns)),perl=TRUE))
}