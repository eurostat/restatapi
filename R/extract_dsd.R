#' @title Extract values from XML 
#' @description Extracts the values from the XML Data Structure Definition (DSD) file
#' @param concept a character vector with concept ids  
#' @param dsd_xml an XML file with DSD content
#' @export 
#' @details It is a subfunction to use in the \code{\link{get_eurostat_dsd}} function.
#' @return a matrix with 3 columns if the concepts has code list in the DSD file
#' @examples 
#' \dontshow{
#' options(mc.cores=min((parallel::detectCores()),2))
#' }
#' dsd_url<-"http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_nama_10_a10_e"
#' dsd_xml<-xml2::read_xml(dsd_url)
#' extract_dsd("GEO",dsd_xml)
#' 
#' 

extract_dsd<-function(concept,dsd_xml){
  xml_clc<-xml2::xml_attr(xml2::xml_find_all(dsd_xml,paste0('//str:Codelist[@id="CL_',concept,'"]/str:Code')),"id")
  if (length(xml_clc)>0){
    xml_cln<-xml2::xml_text(xml2::xml_find_all(dsd_xml,paste0('//str:Codelist[@id="CL_',concept,'"]/str:Code/com:Name')))
    cbind(concept,xml_clc,xml_cln)}
}