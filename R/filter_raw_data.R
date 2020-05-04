#' @title Filter raw data locally  
#' @description Filter downloaded full raw dataset on local computer if the \code{\link{get_eurostat_data}} has not provide data due to too large datasets for the REST API.
#' @param raw_data an input data.table dataset resulted from the call of the \code{\link{get_eurostat_raw}} function 
#' @param filter_table a data table with values for the concepts or time to be filtered out
#' @param date_filter if \code{TRUE} the filter table should be applied to the \code{time} columns of the \code{raw_data}. The default is \code{FALSE}, 
#'        in this case the filters applied to the other columns of \code{raw_data}.
#' @export 
#' @details It is a sub-function to use in the \code{\link{get_eurostat_data}} to filter data if the direct response from REST API did not provide data because of to large data set. 
#'          The \code{filter_table} contains always at least two columns. In case if \code{date_filter=TRUE} then the two columns should have the following names:
#'          \tabular{ll}{
#'            \code{sd} \tab Starting date to be included, where date is formatted yyyy[-mm][-dd] \cr
#'            \code{ed} \tab End date of the period to be included in the dataset formatted yyyy[-mm][-dd] 
#'            }  
#'          In this case these conditions are applied to the time column of the the \code{raw_data} data.table. 
#'          In case if \code{date_filter=FALSE} then the columns should have the following names:
#'          \tabular{ll}{
#'            \code{concept} \tab Eontaining concept names, which is a column name in the \code{raw_data} data.table \cr
#'            \code{code} \tab A possible code under the given concept, which is a value in the column of the \code{raw_data} data.table defined by the concept in the \code{filter_table}   
#'            }  
#'            
#' @return a filtered data.table containing only the rows of \code{raw_data} which fulfills the conditions in the \code{filter_table} 
#' @seealso \code{\link{get_eurostat_raw}}, \code{\link{search_eurostat_dsd}}, \code{\link{get_eurostat_data}}, \code{\link{create_filter_table}}
#' @examples 
#' \dontshow{
#' 
#' }
#' \donttest{
#' id<-"tus_00age"
#' rd<-get_eurostat_raw(id)
#' dsd<-get_eurostat_dsd(id)
#' ft<-create_filter_table(c("TIME_SP","Hungary",'T'),FALSE,dsd)
#' filter_raw_data(rd,ft)
#' }
#' 

filter_raw_data<-function(raw_data,filter_table,date_filter=FALSE){
  .datatable.aware=TRUE
  ft<-time<-NULL
  if (date_filter){
    raw_data$ft<-as.character(raw_data$time)
    #raw_data[grepl("^\\d{4}$",time),time:=paste0(time,"-01-01")]
    raw_data[grepl("[M]\\d\\d$",time),ft:=paste0(gsub('[M]',"-",time),"-01")]
    raw_data[grepl("[MD]",time),ft:=gsub('[MD]',"-",time)]
    raw_data[grepl("Q",time),ft:=paste0(substr(time,1,4),"-",lapply(substr(time,6,6),function(x){if (x<"4"){"0"}else{""}}),as.character((as.numeric(substr(time,6,6))-1)*3+1))]
    raw_data[grepl("S",time),ft:=paste0(substr(time,1,4),"-0",as.character((as.numeric(substr(time,6,6))-1)*6+1),"-01")]
    data_out<-data.table::rbindlist(lapply(1:nrow(filter_table),function (x){raw_data[(ft>=filter_table$sd[x] & ft<=filter_table$ed[x])]}))
    data_out<-data_out[,!'ft'][]
  }else{
    filter_table<-unique(filter_table[,c("concept","code")])
    data.table::setnames(raw_data,colnames(raw_data),toupper(colnames(raw_data)))
    concepts<-unique(filter_table$concept)
    data_out<-raw_data[eval(parse(text=paste(lapply(concepts,function(concept){
      paste("(",paste0(filter_table$concept[filter_table$concept==concept],"==",'"',filter_table$code[filter_table$concept==concept],'"',collapse=" | "),")")
    }),collapse=" & ")))][]
    data.table::setnames(data_out,colnames(data_out),tolower(colnames(data_out)))
  }
  data_out[]
}