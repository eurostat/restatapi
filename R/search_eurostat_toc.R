#' @title Search in Eurostat datasets titles, units and short description
#' @description Lists names of dataset from Eurostat with the particular
#' pattern in the title, units or short description.
#' @details Downloads the list of all tables and datasets available in the 
#' Eurostat database and returns all the details from the table of contents of the tables/datasets that contains particular
#' pattern in the dataset title, unit or short description. E.g. all tables/datasets  mentioning
#' 'energy'.
#' @param pattern Character string to search for in the table of contents of Eurostat tables/datasets
#' @param lang a character string either \code{en}, \code{de} or \code{fr} to define the language version for the table of contents. The default is \code{en} - English.
#' @param verbose A boolean with default \code{FALSE}, so detailed messages (for debugging) will not printed.
#'         Can be set also with \code{options(restatapi_verbose=TRUE)}
#' @param ... other parameters to pass to the \code{grep} function.
#' @return A table with the following columns:
#'    \tabular{ll}{
#'      \code{title} \tab The name of dataset/table in the language provided by the \code{lang} parameter \cr
#'      \code{code} \tab The codename of dataset/table which can be used by the \code{get_eurostat} function \cr
#'      \code{type} \tab The type of information: 'dataset' or 'table' \cr
#'      \code{lastUpdate} \tab The date when the data was last time updated for tables and datasets\cr
#'      \code{lastModified}\tab The date when the structure of the dataset/table was last time modified\cr
#'      \code{dataStart}\tab The start date of the data in the dataset/table\cr
#'      \code{dataEnd}\tab The end date of the data in the dataset/table\cr
#'      \code{values}\tab The number of values in the dataset/table.\cr
#'      \code{unit}\tab The unit name for tables in the language provided by the \code{lang} parameter. For dataset it is empty. \cr
#'      \code{shortDescription}\tab The short description of the values for tables in the language provided by the \code{lang} parameter. For dataset it is empty.\cr
#'      \code{metadata.html}\tab The link to the metadata in html format.\cr
#'      \code{metadata.sdmx}\tab The link to the metadata in SDMX format.\cr
#'      \code{downloadLink.tsv}\tab The link to the whole dataset/table in tab separated values format in the bulk download facility. \cr
#'      \code{downloadLink.sdmx}\tab The link to the whole dataset/table in SDMX format in the bulk download facility.
#'    }
#'    The value in the \code{code} column can be used as an id in the \code{\link{get_eurostat_data}}, \code{\link{get_eurostat_bulk}}, \code{\link{get_eurostat_raw}} and \code{\link{get_eurostat_dsd}} functions.
#' @export
#' @seealso \code{\link{search_eurostat_dsd}}, \code{\link{get_eurostat_data}}, \code{\link{get_eurostat_toc}}
#' @examples 
#' \dontshow{
#' if (parallel::detectCores()<=2){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }    
#' }
#' \donttest{
#'   head(search_eurostat_toc("energy"))
#'   head(search_eurostat_toc("energie",lang="de",ignore.case=TRUE))
#' }


search_eurostat_toc <- function(pattern,lang="en",verbose=FALSE,...) {
  tmp<-get_eurostat_toc(lang=lang,verbose=verbose)
  tmp[grepl(pattern,tmp$title,...)|grepl(pattern,tmp$unit,...)|grepl(pattern,tmp$shortDescription,...), ]
}
