#' @title Extract values from XML 
#' @description Extracts the values of a node from the Eurostat XML Table of contents (TOC) file
#' @param ns An XML nodeset from the XML TOC file 
#' @export
#' @details It is a subfunction to use in the \code{\link{get_eurostat_toc}} function.
#' @return a character vector with all the values of the nodeset.
#' @examples 
#' \dontshow{
#' options(mc.cores=min((parallel::detectCores()),2))
#' }
#' cfg<-get("cfg",envir=.restatapi_env) 
#' rav<-get("rav",envir=.restatapi_env)
#' toc_endpoint<-eval(parse(text=paste0("cfg$TOC_ENDPOINT$'",rav,"'$ESTAT$xml")))
#' xml_leafs<-xml2::xml_find_all(xml2::read_xml(toc_endpoint),".//nt:leaf")
#' values<- extract_toc(xml_leafs[1])
#' 
#' 

extract_toc<-function(ns){
  sub("<.*","",sub(".*?>","",as.character(xml2::xml_children(ns)),perl=T))
}