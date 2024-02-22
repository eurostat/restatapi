#' @title Extract the Data Structure Definition content from SDMX XML 
#' @description Extracts values from the XML Data Structure Definition (DSD) file
#' @param concept a character vector with a concept id  
#' @param dsd_xml an XML file with DSD content
#' @param lang a character string either \code{en}, \code{de} or \code{fr} to define the language version for the name column 
#' of the DSD. It is used only in the new API. The default is \code{en} - English.
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
#' \donttest{
#' id<-"med_rd6"
#' cfg<-get("cfg",envir=restatapi::.restatapi_env)
#' rav<-get("rav",envir=restatapi::.restatapi_env)
#' dsd_url <- paste0(eval(
#'      parse(text=paste0("cfg$QUERY_BASE_URL$'",rav,"'$ESTAT$metadata$'2.1'$datastructure"))
#'    ),"/",eval(
#'      parse(text=paste0("cfg$QUERY_PRIOR_ID$'",rav,"'$ESTAT$metadata"))
#'    ),id,"?",eval(
#'      parse(text=paste0("cfg$QUERY_PARAMETERS$'",rav,"'$metadata[2]"))
#'    ),"=",eval(
#'      parse(text=paste0("cfg$DATAFLOW_REFERENCES$'",rav,"'$datastructure[1]"))
#'    )
#'  )
#' if (!(grepl("amzn|-aws|-azure ",Sys.info()['release']))) options(timeout=2)
#' tryCatch({
#'   dsd_xml<-xml2::read_xml(dsd_url)}, 
#'   error=function(e){
#'   message("Unable to download the xml file.\n",e)}, 
#'   warning=function(w){
#'   message("Unable to download the xml file.\n",w)}) 
#' if (exists("dsd_xml")) extract_dsd("FREQ",dsd_xml) 
#' options(timeout=2)
#' }

extract_dsd<-function(concept=NULL,dsd_xml=NULL,lang="en"){
  rav<-get("rav",envir=restatapi::.restatapi_env)
  if (getOption("restatapi_verbose",FALSE))  {message("\nextract_dsd - API version:",rav)}
  
  xml_string_clc<-switch(rav,
                      "1" = paste0('//str:Codelist[@id="CL_',concept,'"]/str:Code'),
                      "2" = paste0('//s:Codelist[@id="',toupper(concept),'"]/s:Code')
    
  )
  xml_string_cln<-switch(rav,
                      "1" = paste0('//str:Codelist[@id="CL_',concept,'"]/str:Code/com:Name'),
                      "2" = paste0('//s:Codelist[@id="',toupper(concept),'"]/s:Code/c:Name[@xml:lang="',lang,'"]')
                        
  )
  if (getOption("restatapi_verbose",FALSE))  {message("extract_dsd - xml_string_clc:",xml_string_clc,"\nextract_dsd - xml_string_cln:",xml_string_cln)}

  if (is.null(dsd_xml)|is.null(concept)){
    message("The XML file or the concept is missing.")
    return(NULL)
  } else {
    if (Sys.info()[['sysname']]=='Windows'){dsd_xml<-xml2::as_xml_document(dsd_xml)}
    xml_clc<-xml2::xml_attr(xml2::xml_find_all(dsd_xml,xml_string_clc),"id")
    if (length(xml_clc)>0){
      xml_cln<-xml2::xml_text(xml2::xml_find_all(dsd_xml,xml_string_cln))
      cbind(concept,xml_clc,xml_cln)
    }
  } 
}  