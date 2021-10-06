#' @title Extract the Data Structure Definition content from SDMX XML 
#' @description Extracts values from the XML Data Structure Definition (DSD) file
#' @param concept a character vector with a concept id  
#' @param dsd_xml an XML file with DSD content
#' @export 
#' @details It is a sub-function to use in the \code{\link{get_eurostat_dsd}} function.
#' @return  a matrix with 3 columns if the provided \code{concept} has a code list in the DSD file. The first column is the provided \code{concept}. The second column 
#'         is the possible codes under the given \code{concept}. The last column is the name/description for the code in the second column, which can be used as labels.
#' @examples 
#' \dontshow{
#' if (parallel::detectCores()<=2){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }    
#' }
#' dsd_url<-"https://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_med_rd6"
#' tryCatch({
#'   dsd_xml<-xml2::read_xml(dsd_url)}, 
#'   error=function(e){
#'   message("Unable to download the xml file.\n",e)}, 
#'   warning=function(w){
#'   message("Unable to download the xml file.\n",w)}) 
#' if (exists("dsd_xml")) {extract_dsd("GEO",dsd_xml)} 
#' 

extract_dsd<-function(concept=NULL,dsd_xml=NULL){
  if (is.null(dsd_xml)|is.null(concept)){
    message("The XML file or the concept is missing.")
    return(NULL)
  } else {
    if (Sys.info()[['sysname']]=='Windows'){dsd_xml<-xml2::as_xml_document(dsd_xml)}
    xml_clc<-xml2::xml_attr(xml2::xml_find_all(dsd_xml,paste0('//str:Codelist[@id="CL_',concept,'"]/str:Code')),"id")
    if (length(xml_clc)>0){
      xml_cln<-xml2::xml_text(xml2::xml_find_all(dsd_xml,paste0('//str:Codelist[@id="CL_',concept,'"]/str:Code/com:Name')))
      cbind(concept,xml_clc,xml_cln)
    }  
  }
}  