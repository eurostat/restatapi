#' @title Create a filter table  
#' @description Create filter table from the \code{filters} and \code{date_filter} strings parameters of the \code{\link{get_eurostat_data}} to be used in the \code{\link{filter_raw_data}} function for filtering by query or on the local computer.
#' @param filters a string, a character or numeric vector or a named list containing words to filter by the different concepts, geographical location or time values.
#'        The words can be any word, Eurostat variable code, or value which are in the Data Structure Definition (DSD) and can be retrieved by the \code{\link{search_eurostat_dsd}} function. 
#'        If a named list is used, then the name of the list elements should be the concepts from the DSD and the provided values will be used to filter the dataset for the given concept.
#'        The default is \code{NULL}, in this case no filter table is created. To filter by time see \code{date_filter} below.
#'        In case for filtering for time values, the date shall be defined as character string, and it should follow the format yyyy[-mm][-dd], where the month and the day part is optional.
#' @param date_filter a logical value. If \code{TRUE} the filter table is genrated only for the time dimension. The default is \code{FALSE}, 
#'        in this case a (\code{dsd}) should be provided which will be searched for the values given in the \code{filters}. 
#' @param dsd a table containing a DSD of an Eurostat dataset which can be retreived by the \code{\link{get_eurostat_dsd}} function. 
#' @param exact_match a logical value with the default value \code{TRUE}, if the strings provided in \code{filters} shall be matched exactly as it is or as a pattern in the DSD. 
#' @param verbose a logical value with default \code{FALSE}, so detailed messages (for debugging) will not printed.
#'         Can be set also with \code{options(restatapi_verbose=TRUE)}
#' @param ... further arguments to the for \code{\link{search_eurostat_dsd}} function, e.g.: \code{ignore.case} or \code{name}. 
#'        The \code{ignore.case} has the default value \code{FALSE}, then the strings provided in \code{filters} are matched as is, 
#'        otherwise the case of the letters is ignored. If the \code{name=FALSE} then the pattern(s) provided in the \code{filters}
#'        argument is only searched in the code column of the DSD, and the names of the codes will not be searched. 
#' @export 
#' @details It is a sub-function to use in the \code{\link{get_eurostat_data}} to generate url for the given \code{filters} and \code{date_filter} in that function. The output can be used also for filtering data 
#'          on the local computer with the \code{\link{get_eurostat_raw}} and \code{\link{filter_raw_data}} function, if the direct response from REST API did not provide data because of too large data set. 
#' @return a data.table containing in each row a distinct filtering condition to be applied to a raw Eurostat datatable or generate specific query.
#' 
#'          If \code{date_filter=TRUE}, the output data table contains two columns with the following names:
#'          \tabular{ll}{
#'          \code{sd} \tab Starting date to be included in the filtered dataset, where date is formatted yyyy[-mm][-dd]\cr
#'          \code{ed} \tab End date of the period to be included in the filtered dataset, where the date is formatted yyyy[-mm][-dd] 
#'          }  
#'          In case \code{date_filter=FALSE}, the output tables have the following four columns:
#'          \tabular{ll}{   
#'          \code{pattern} \tab Containing those parts of the \code{filters} string where the string part (pattern) was found in the \code{dsd}\cr
#'          \code{concept} \tab The name of the concepts corresponding to the result in the code/name column where the pattern
#'          was found in the data structure definition\cr
#'          \code{code} \tab The list of codes where the pattern was found, or the code of a name (description of the code) 
#'          where the pattern appears\cr
#'          \code{name} \tab The name (description of the code) which can be used as label for the code where the pattern was
#'          found, or the name (description of the code) of the code where the pattern appears 
#'          }
#' @seealso \code{\link{get_eurostat_raw}}, \code{\link{search_eurostat_dsd}}, \code{\link{get_eurostat_data}}, \code{\link{filter_raw_data}}
#' @examples 
#' \dontshow{
#' if (parallel::detectCores()<=2){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }    
#' cfg<-get("cfg",envir=restatapi::.restatapi_env) 
#' rav<-get("rav",envir=restatapi::.restatapi_env)
#' }
#' \donttest{
#' options(timeout=2)
#' dsd<-get_eurostat_dsd("avia_par_me") 
#' create_filter_table(c("KYIV","hu","Quarterly"),dsd=dsd,exact_match=FALSE,ignore.case=TRUE)
#' create_filter_table(c("KYIV","LHBP","Monthly"),dsd=dsd,exact_match=FALSE,name=FALSE)
#' create_filter_table(c("2017-03",
#'                       "2001-03:2005",
#'                       "<2000-07-01",
#'                       2012:2014,
#'                       "2018<",
#'                       20912,
#'                       "<3452<",
#'                       ":2018-04>",
#'                       "2<034v",
#'                       "2008:2013"),
#'                     date_filter=TRUE,
#'                     verbose=TRUE)
#' options(timeout=60)
#' }

create_filter_table <- function(filters,date_filter=FALSE,dsd=NULL,exact_match=TRUE,verbose=FALSE,...) {
  .datatable.aware=TRUE
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  
  ft<-sd<-ed<-NULL
  if (!date_filter & is.null(dsd)){
    message("The DSD is missing from the create_filter_table function.")
  } else{
    if (verbose) {message("\ncreate_filter_table - filters class: ",class(filters),"; size: ",length(filters),"; filters:",filters)}
    if (class(filters)=="name") {
      try(filters<-local(filters),silent=verbose)
    }  
    # loop<-TRUE
    time_formats<-c("^((?:19|20|21)\\d\\d)$","^^((?:19|20|21)\\d\\d)-(0[1-9]|1[012])$","^^((?:19|20|21)\\d\\d)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$")
    if (date_filter) {
      if (verbose) {message("create_filter_table - filters: ",filters,"; is numeric: ",is.numeric(filters),"; call parents: ",length(sys.calls())-1)}
      if (length(sys.calls())>1){
        if (any(grepl("get_eurostat_data",as.character(sys.calls()),perl=TRUE))){
          df<-as.character(filters)
        } else{
          df<-as.character(substitute(filters))
        }
      } else {
        df<-as.character(filters)
      }
      if (verbose) {message("length df: ",length(df)," -*- df: ",paste(df,collapse=", "))}
      if (df[1]=="c"){
        df<-df[2:length(df)]
      } else {
        # df<-as.character(parse(text=deparse(filters)))
      }
      if (any(grepl("[^0-9\\-\\:<>]",df,perl=TRUE))){
        df<-gsub("[^0-9\\-\\:<>]","",df,perl=TRUE)
        df<-df[df!=""]
        message("The date filter had invalid character (not 0-9, '-', '<', '>' or ':'). Those characters removed from the date filter.")
      } 
      if (verbose){message(paste("create_filter_table - ",df,collapse=", ")," date filter length: ",length(df),", nchar date_filter: ",paste(nchar(df),collapse=","))}
      dft<-data.table::rbindlist(lapply(df, function(sdf) {
        if (nchar(gsub("[^:<>]","",sdf,perl=TRUE))>1){
          res<-NULL
          if(verbose){message(paste0("Could not parse date filter: '",sdf,"'. This date filter is ignored."))}
        } else {
          if (grepl(":",sdf,perl=TRUE)){
            dates<-unlist(strsplit(sdf,":"))
            if (all(sapply(dates,check_tf,tf=time_formats))){
              res<-list(sd=min(dates),ed=max(dates))
            } else{
              res<-NULL
              if(verbose){message(paste0("Could not parse date filter: '",paste0(dates,collapse=":"),"' (at least one date not in yyyy[-mm][-dd] format or incorrect date value). The date filter is ignored."))}
            }
          } else if (grepl("<|>",sdf,perl=TRUE)){
            if(check_tf(gsub("<|>","",sdf,perl=TRUE),time_formats)){
              if (grepl("^<|>$",sdf,perl=TRUE)){
                res<-list(sd=0,ed=gsub("<|>","",sdf,perl=TRUE))  
              } else if (grepl("^>|<$",sdf,perl=TRUE)){
                res<-list(sd=gsub("<|>","",sdf,perl=TRUE),ed=Inf)
              } else {
                res<-NULL
                if (verbose) {message(paste0("Could not parse date filter: '", sdf,"' not in [<>]yyyy[-mm][-dd][<>] format or incorrect date value. The date filter is ignored."))}
              }
            } else {
              res<-NULL
              if(verbose){message(paste0("Could not parse date filter: '",sdf,"' (not in yyyy[-mm][-dd] format or incorrect date value. The date filter is ignored."))}
            }
          } else{
            if(check_tf(sdf,time_formats)){
              res<-list(sd=sdf,ed=sdf)  
            } else {
              res<-NULL
              if (verbose) {message(paste0("Could not parse date filter: '",sdf,"' not in [<>]yyyy[-mm][-dd][<>] format or incorrect date value. The date filter is ignored."))}
            }
          }  
        }
        return(res)
      }),fill=TRUE)
      if(!is.null(dft)){
        if(nrow(dft)>0){
          dft[nchar(sd)==4,sd:=paste0(sd,"-01-01")]
          dft[nchar(ed)==4,ed:=paste0(ed,"-12-31")]
          dft[nchar(sd)==7,sd:=paste0(sd,"-01")]
          dft[grepl("^\\d{4}-(01|03|05|07|08|10|12)$",ed),ed:=paste0(ed,"-31")]
          dft[grepl("^\\d{4}-(04|06|09|11)$",ed),ed:=paste0(ed,"-30")]
          dft[,sd:=gsub("-02-(29|3[01])$","-02-28",sd)]
          dft[,ed:=gsub("-02-(29|3[01])$","-02-28",ed)]
          # dft[]
          dft<-dft[order(sd,ed)]
          dft<-dft[c(TRUE, !(utils::tail(ed, -1) <= utils::head(ed, -1)))]
          if (nrow(dft)>2){
            # while(loop){
            dft<-dft[, list(sd=min(sd), ed=max(ed)),by=list(group=cumsum(c(1, as.character(as.Date(utils::tail(sd, -1))-1) > utils::head(ed, -1))))][,c("sd","ed")]
            # if (nrow(dft)==1) {loop=FALSE} else {loop<-any(utils::tail(dft$sd, -1) > as.character(as.Date(utils::head(dft$ed, -1))+1))}
            # dft[]
            # }
          }
          ft<-dft[, list(sd=min(sd), ed=max(ed)),by=list(group=cumsum(c(1, as.character(as.Date(utils::tail(sd, -1))-1) > utils::head(ed, -1))))][,c("sd","ed")]
          # ft<-ft[,c("sd","ed")]
        }
      }
      if (!is.null(ft)){if (nrow(ft)==1 & all(ft$sd==0) & all(ft$ed==Inf)) { ft<-NULL}}
    } else if (is.null(dsd)){
      message('No DSD were provided.')
      ft<-NULL
    } else {
      dsd<-dsd[!(dsd$concept %in% c("OBS_FLAG","OBS_STATUS")),]
      if (is.null(names(filters))){
        if (length(filters)==1 & length(gregexpr("\\.",filters)[[1]])==length(unique(dsd$concept))-1){
          lfilters<-unlist(strsplit(filters,"\\."))
          names(lfilters)<-unique(dsd$concept)
          ft<-data.table::rbindlist(lapply(filters,function (x){
            data.table::data.table(pattern=unlist(strsplit(x,"\\+")),concept=names(x),code=unlist(strsplit(x,"\\+")),name="")
          }))
        } else{
          ft<-data.table::rbindlist(lapply(filters,search_eurostat_dsd,dsd=dsd,exact_match=exact_match,...))
        }
      } else{
        concepts<-names(filters)
        ft<-data.table::rbindlist(lapply(1:length(filters),function (x,f,d){
          do.call(rbind,lapply(unlist(f[x]),search_eurostat_dsd,dsd=d[d$concept==toupper(concepts[x]),],exact_match=exact_match,...))
        },f=filters,d=dsd))
      }
    }  
  }
  
  ft[]
}

check_tf<-function(x,tf){
  any(sapply(tf,grepl,x=x,perl=TRUE))
}
