#' @title Extract values from XML 
#' @description Extracts the values of a node from the Eurostat XML Table of contents (TOC) file
#' @param ns An XML nodeset from the XML TOC file 
#' @export
#' @details It is a subfunction to use in the \code{\link{get_eurostat_toc}} function.
#' @return a character vector with all the values of the nodeset.
#' @examples 
#' \dontrun{
#' values<- extract_toc(toc)
#' }
#' 

extract_toc<-function(ns){
  sub("<.*","",sub(".*?>","",as.character(xml2::xml_children(ns)),perl=T))
}