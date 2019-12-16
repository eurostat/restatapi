#' @title Search the downloaded Data Structure Definition of a dataset
#' @description Search the Data Structure Definition (DSD) of a Eurostat dataset for a given pattern. It returns 
#'              the rows where the pattern appears in the code and name column of the output of the \code{\link{get_eurostat_dsd}} 
#'              function. 
#' @param pattern a character string or a vector of character string.
#' @param dsd a table with the character string with the id of the dataset. 
#' @param name a boolean with the default value \code{TRUE}, if the search shall look for the pattern in the name of the code.
#'             If the value \code{FALSE}, then only the 'code' column of the DSD will be  searched.
#' @param exact_match a boolean with the default value \code{FALSE}, if the strings provided in \code{pattern} shall be matched exactly as it is or as a pattern. 
#' @param ... additional arguments to the \code{grep} function like \code{ignore.case=TRUE} if the pattern should be searched case sensitive or not. 
#'            The default value for \code{ignore.case} is \code{FALSE}. 
#' @return If the pattern found then the function returns table with the 4 columns:
#'    \tabular{ll}{
#'      \code{pattern} \tab The pattern which was searched \cr
#'      \code{concept} \tab The name of the concepts in the data structure \cr
#'      \code{code} \tab The list of codes where the pattern was found, or the code of a name where the pattern appears \cr
#'      \code{name} \tab The name/description of the code where the pattern found, or the name of the code where the pattern appears 
#'    }
#'    Otherwise returns the value \code{NULL}.
#' @export
#' @seealso \code{\link{get_eurostat_dsd}}, \code{\link{search_eurostat_toc}}.
#' @details The function returns the line(s) where the searched pattern appears in the code or in the name column.
#'  
#' @examples 
#' \dontshow{
#' if (parallel::detectCores()<=2){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }    
#' cfg<-get("cfg",envir=.restatapi_env) 
#' rav<-get("rav",envir=.restatapi_env)
#' }
#' dsd_example<-get_eurostat_dsd("nama_10_gdp",verbose=TRUE)
#' search_eurostat_dsd("EU",dsd_example)
#' search_eurostat_dsd("EU",dsd_example,ignore.case=TRUE)
#' search_eurostat_dsd("EU27_2019",dsd_example,name=FALSE)
#' search_eurostat_dsd("EU27_2019",dsd_example,exact_match=TRUE)
#' 


search_eurostat_dsd <- function(pattern,dsd=NULL,name=TRUE,exact_match=FALSE,...) {
  if (is.null(dsd)){
    message('No DSD were provided.')
    sr<-NULL
  } else if (is.null(pattern)) {
    sr<-NULL
  } else if (length(pattern)>1){
    message("The 'pattern' (",paste(pattern,collapse=", "),") has length > 1. In this case use something like 'do.call(rbind,lapply(pattern,search_eurostat_dsd,dsd=dsd))'.")
    sr<-NULL
  } else {
    if (exact_match){
      pattern<-paste0("^",pattern,"$")
      pattern<-sub("^\\^{2}","\\^",sub("\\${2}$","\\$",pattern))
    }
    if (all(c("concept","code","name") %in% colnames(dsd))){
      if (!name) {
        rn<-unique(c(grep(pattern,dsd$code,...)))
      } else {
        rn<-unique(c(grep(pattern,dsd$code,...),grep(pattern,dsd$name,...)))
      }
      if (length(rn>0)){
        sr<-data.frame(pattern,dsd[rn,],stringsAsFactors=FALSE)  
      }else{
        sr<-NULL
      }
    }else{
      message("The DSD does not contain all the columns.")
      sr<-NULL
    }
  }
  sr
}