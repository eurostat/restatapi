#' @title Search the downloaded the Data Structure Definition of a dataset
#' @description Search the Data Structure Definition (DSD) of a Eurostat dataset for a given pattern. It returns the rows where the pattern appears in the code and name column of the output of the \code{\link{get_eurostat_dsd}} function. 
#' @param pattern a character string or a vector of character string.
#' @param dsd a table with the character string with the id of the dataset.  
#' @param ignore.case a boolean if the pattern is case sensitive or not. The default is \code{TRUE}. 
#' #' @return If the pattern found then the function returns table with the 4 columns:
#'    \tabular{ll}{
#'      \code{pattern} \tab The pattern which was searched \cr
#'      \code{concept} \tab The name of the concepts in the data structure \cr
#'      \code{code} \tab The list of codes where the pattern was found, or the code of a name where the pattern appears \cr
#'      \code{name} \tab The name/description of the code where the pattern found, or the name of the code where the pattern appears 
#'    }
#'    Otherwise returns the value \code{FALSE}.
#' @export
#' @seealso \code{\link{get_eurostat_dsd}}, \code{\link{search_eurostat_toc}}.
#' @details The function returns the line(s) where the searched pattern appears in the code or in the name column.
#'  
#' @examples 
#' \dontshow{
#' if ((parallel::detectCores()<2)|(Sys.info()[['sysname']]=='Windows')){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }    
#' cfg<-get("cfg",envir=.restatapi_env) 
#' rav<-get("rav",envir=.restatapi_env)
#' }
#' dsd_example<-get_eurostat_dsd("nama_10_gdp",verbose=TRUE)
#' search_eurostat_dsd("EU",dsd_example)
#' search_eurostat_dsd("EU",dsd_example,ignore.case=FALSE)
#' 


search_eurostat_dsd <- function(pattern,dsd=NULL,ignore.case=TRUE) {
  if (is.null(dsd)){
    message('No DSD were provided.')
    sr<-FALSE
  } else if (is.null(pattern)) {
    sr<-FALSE
  } else {
    if (all(c("concept","code","name") %in% colnames(dsd))){
      rn<-unique(c(grep(pattern,dsd$code,ignore.case=ignore.case),grep(pattern,dsd$name,ignore.case=ignore.case)))
      if (length(rn>0)){
        sr<-data.frame(pattern,dsd[rn, ],stringsAsFactors=FALSE)  
      }else{
        sr<-FALSE
      }
    }else{
      stop("The DSD does not contain all the columns.")      
    }
  }
  sr
}