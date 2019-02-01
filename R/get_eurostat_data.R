#' @title Download/extract Eurostat Data
#' @description Download full or partial data set from \href{https://ec.europa.eu/eurostat/}{Eurostat} database.
#' @param id A code name for the dataset of interest.
#'        See \code{\link{search_eurostat_toc}} for details how to get an id.
#' @param filters a string or a character vector containing words to filter by the different concepts or geographical location.
#'        If filter applied only part of the dataset is downloaded through the API. The words can be  
#'        any words, Eurostat variable codes, and values available in the DSD \code{\link{search_eurostat_dsd}}. 
#'        The default is \code{NULL}, in this case the whole dataset is returned via the bulk download. To filter by time see \code{date_filter} below.
#'        If after filtering still the dataset has more observations than the limit per query via the API, then the bulk download is used to retrieve the data. 
#' @param ignore.case a boolean with the default value \code{FALSE}, if the strings provided in \code{filters} shall be matched as is or the case can be ignored.  
#' @param date_filter a vector which can be numeric or character containing dates to filter the dataset.
#'        If date filter applied only part of the dataset is downloaded through the API. 
#'        The default is \code{NULL}, in this case the whole dataset is returned via the bulk download.
#'        If after filtering still the dataset has more observations than the limit per query via the API, then the bulk download is used to retrieve the data. 
#' @param label a boolean with the default \code{FALSE}. If it is \code{TRUE} then the code values are replaced by the name from the Data Structure Definition (DSD) \code{\link{get_eurostat_dsd}}.
#'         For example instead of "D1110A", "Raw cows' milk from farmtype" is used or "HU32" is replaced by "Észak-Alföld".
#' @param select_freq a character symbol for a time frequency when a dataset has multiple time
#'        frequencies. Possible values are:
#'    	  A = annual, S = semi-annual, H = half-year, Q = quarterly, M = monthly, W = weekly, D = daily. 
#'    	  The default is \code{NULL} as most datasets have just one time
#'        frequency and in this case if there are multiple frequencies, then only the most common frequency kept.
#'        If all the frequencies needed the \code{\link{get_eurostat_raw}} can be used.
#' @param cache a logical whether to do caching. Default is \code{TRUE}. Affects 
#'        only queries without filtering. If \code{filters} or \code{date_filter} is used then there is no caching.
#' @param update_cache a logical with a default value \code{FALSE}, whether to update the data in the cache. Can be set also with
#'        \code{options(restatapi_update = TRUE)}
#' @param cache_dir a path to a cache directory. The \code{NULL} (default) uses the memory as cache. 
#'        If the folder \code{cache_dir} directory does not exist it saves in the 'restatapi' directory 
#'        under the temporary directory from \code{tempdir()}. Directory can also be set with
#'        \code{option(restatapi_cache_dir=...)}.
#' @param compress_file a logical whether to compress the
#'        RDS-file in caching. Default is \code{TRUE}.
#' @param stringsAsFactors if \code{TRUE} (the default) variables are not numeric then they are
#'        converted to factors. If the value \code{FALSE}
#'        they are returned as a characters.
#' @param keep_flags a logical whether the observation status (flags) - e.g. "confidential",
#'        "provisional", etc. - should be kept in a separate column or if they
#'        can be removed. Default is \code{FALSE}. For flag values see: 
#'        \url{http://ec.europa.eu/eurostat/data/database/information}.
#' @param verbose A boolean with default \code{FALSE}, so detailed messages (for debugging) will not printed.
#'         Can be set also with \code{options(restatapi_verbose=TRUE)}
#' @param ... further argument for \code{\link{load_cfg}} function.
#' @export
#' 
#' @details Data sets are downloaded from the Eurostat Web Services 
#' \href{https://ec.europa.eu/eurostat/web/sdmx-web-services}{SDMX API} if there is a filter otherwise the 
#' \href{http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing}{the Eurostat bulk download facility} is used.
#' If only the table \code{id} is given, the whole table is downloaded from the
#' bulk download facility. If also \code{filters} or \code{date_filter} is defined then the SDMX API is
#' used. In case after filtering the dataset has more rows than the limitation of the SDMX API (1 million values at one time) then the bulk download is used to retrieve the whole dataset .
#' 
#' By default all datasets cached as they are often rather large. 
#' The datasets cached in memory (default) or can be stored in a temporary directory if \code{cache_dir} or \code{option(restatpi_cache_dir)} is defined.
#' The cache can be emptied with \code{\link{clean_restatapi_cache}}.
#' 
#' The \code{id}, is a value from the \code{code} column of the table of contents (\code{\link{get_eurostat_toc}}), and can be searched for with the \code{\link{search_eurostat_toc}} function. The id value can be retrieved from the \href{http://ec.europa.eu/eurostat/data/database}{Eurostat database}
#'  as well. The Eurostat
#' database gives codes in the Data Navigation Tree after every dataset
#' in parenthesis.
#' 
#' Filtering can be done by the codes as described in the API documentation providing in the correct order and connecting with "." and "+". 
#' If we do not know the codes we can filter based on words or by the mix of the two putting in a vector like \code{c("AT$","Belgium","persons","Total")}. Be careful that the filter is case sensitive, if you do not know exactly you can use the option \code{ignore.case=T}, but it can include unwanted elements as well. 
#' We do not have to worry about the correct order it will be put in the correct place based on the DSD. In the \code{filters} parameter regular expressions can be used as well. 
#' 
#' The \code{date_filter} shall be a string in the format yyyy[-mm][-dd]. The month and the day part is optional, but if we use the years and we have monthly frequency then all the data for the given year is retrieved.
#' The string can be extended by adding the "<" or ">" to the beginning or to the end of the string. In this case the date filter is treated as range, and the date is used as a starting or end date. The data will include the observation of the start/end date.
#' A single date range can be defined as well by concatenating two dates with the ":", e.g. "2016-08:2017-03-15". The dates can have different length. In this case cannot use the "<" or ">" characters.
#' If there are multiple dates which is not a continuous range, it can be put in vector in any order like c("2016-08",2013-2015,"2017-07-01"). In this case, as well, it is  not possible to use the  "<" or ">" characters.      
#'   
#' @return a data.table. One column for each dimension in the data,
#'         the time column for a time dimension, 
#'         the values column for numerical values and the flags column if the \code{keep_flags=TRUE}.
#'         Eurostat data does not include all missing values. The missing values are dropped if all dimensions are missing
#'         on particular time. 
#' @seealso \code{\link{search_eurostat_toc}},\code{\link{search_eurostat_dsd}}
#' @examples 
#' \dontrun{
#' 
#' dt<-get_eurostat_data("NAMA_10_GDP")
#' dt<-get_eurostat_data("nama_10_gdp",update_cache=TRUE)
#' dt<-get_eurostat_data("nama_10_gdp",cache_dir="/tmp")
#' options(restatapi_update=TRUE)
#' options(restatapi_cache_dir=file.path(tempdir(),"restatapi"))
#' 
#' dt<-get_eurostat_data("avia_gonc", select_freq = "A", cache = FALSE)
#' dt<-get_eurostat_data("agr_r_milkpr",date_filter=2008,keep_flags=T)
#' dt<-get_eurostat_data("avia_par_me",
#'                       filters="BE$",
#'                       date_filter=c(2016,"2017-03","2017-07-01"),
#'                       select_freq="Q",
#'                       label=T)
#' dt<-get_eurostat_data("agr_r_milkpr",filters=c("BE$","Hungary"),date_filter="2007-06<",keep_flags=T)
#' dt<-get_eurostat_data("nama_10_a10_e",
#'                       filters=c("Annual","EU28","Belgium","AT","Total","EMP_DC","person"),
#'                       date_filter=c("2008",2002,2013:2018))
#' dt<-get_eurostat_data("avia_par_me",
#'                       filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",
#'                       date_filter=c("2016-08","2017-07-01"),select_freq="M")
#' }

get_eurostat_data <- function(id,
                         filters=NULL,
                         ignore.case=FALSE,
                         date_filter=NULL,
                         label=FALSE,
                         select_freq=NULL,
                         cache=TRUE,
                         update_cache=FALSE,
                         cache_dir=NULL,
                         compress_file=TRUE,
                         stringsAsFactors=default.stringsAsFactors(),
                         keep_flags=FALSE,
                         verbose=FALSE,...) {
  
  .datatable.aware=TRUE
  restat<-rdat<-drop<-concept<-code<-NULL
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  update_cache<-update_cache|getOption("restatapi_update",FALSE)
  if (!(exists(".restatapi_env"))) {load_cfg(...)}
  cfg<-get("cfg",envir=.restatapi_env) 
  rav<-get("rav",envir=.restatapi_env)
  id<-tolower(id)
  if(!is.null(select_freq)){
    append_sf<-F
    if (is.null(filters)|(length(filters)>1)) {
      append_sf<-T
    } else if (!is.null(filters)) {
      if (grepl("\\.",filters,perl=T)){
        if (grepl("^\\.",filters,perl=T)){
          filters<-paste0(select_freq,filters)
        } else{
          filters<-paste0(select_freq,"+",filters)
        }  
      } else {
        append_sf<-T
      }
    }
    if (append_sf){
      if(select_freq=="A"){
        filters<-c(filters,"Annual")
      } else if (select_freq=="S") {
        filters<-c(filters,"Semi-annual")
      } else if (select_freq=="H") {
        filters<-c(filters,"Half-year")
      } else if (select_freq=="Q") {
        filters<-c(filters,"Quarterly")
      } else if (select_freq=="M") {
        filters<-c(filters,"Monthly")
      } else if (select_freq=="W") {
        filters<-c(filters,"Weekly")
      } else if (select_freq=="D") {
        filters<-c(filters,"Daily")
      } else {
        select_freq<-NULL
        warning("Incorrect frequency selected. It can be 'A', 'S', 'H', 'Q', 'M', 'W' or 'D'. The select_freq parameter will be ignored.")
      }
    }
  }
  if ((!is.null(filters))|(!is.null(date_filter))) {
    if (!is.null(filters)){
      dsd<-get_eurostat_dsd(id)
      dsdorder<-unique(dsd$concept)[1:(length(unique(dsd$concept))-2)]
      if((length(filters)>1)){   
        ft<-do.call(rbind,lapply(filters,search_eurostat_dsd,dsd=dsd,ignore.case=ignore.case))[,2:3]
        ft<-ft[order(match(ft$concept, dsdorder)),]
        filters<-paste0(sapply(dsdorder,gen_ft,ft),collapse=".")
      } else if (length(gregexpr("\\.",filters,perl=T)[[1]])!=(length(dsdorder)-1)){
        if (!is.null(nrow(search_eurostat_dsd(filters,dsd=dsd,ignore.case=ignore.case)))){
          ft<-search_eurostat_dsd(filters,dsd=dsd,ignore.case=ignore.case)[,2:3]
          ft<-ft[order(match(ft$concept, dsdorder)),]
          filters<-paste0(sapply(dsdorder,gen_ft,ft),collapse=".")        
        } else {
          filters<-NULL
        }
      }
    }
    if (!is.null(date_filter)){
      date_filter<-as.character(date_filter)
      if (any(grepl("[^0-9\\-\\:<>]",date_filter,perl=T))){
        date_filter<-NULL
        warning("The date filter has invalid character (not 0-9, '-', '<', '>' or ':'). The date filter is ignored.")
      } else if (length(date_filter)==1)  {
        if (grepl(":",date_filter,perl=T)){
          dates<-unlist(strsplit(date_filter,":"))
          if (length(dates)!=2){
            date_filter<-NULL
            warning("Could not parse date range (more than one ':'). The date filter is ignored.")
          } else if (all(sapply(dates,check_tf,tf=c("^[0-9]{4}$","^[0-9]{4}-[0-9]{2}$","^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))){
            if ((as.numeric(gsub("-","",dates[2]))*(10^(8-nchar(as.numeric(gsub("-","",dates[2]))))))>(as.numeric(gsub("-","",dates[1]))*(10^(8-nchar(as.numeric(gsub("-","",dates[1]))))))){
              date_filter<-paste0("?startPeriod=",dates[1],"&endPeriod=",dates[2])
            } else {
              date_filter<-paste0("?startPeriod=",dates[2],"&endPeriod=",dates[1])
            }
          } else{
            date_filter<-NULL
            warning("The date range has invalid character (only 0-9 and '-' can be used). The date filter is ignored.")
          }
        } else if (check_tf(date_filter,c("^[<>]?[0-9]{4}[<>]?$","^[<>]?[0-9]{4}-[0-9]{2}[<>]?$","^[<>]?[0-9]{4}-[0-9]{2}-[0-9]{2}[<>]?$"))){
          if (grepl("^<[0-9\\-]{4,10}$",date_filter,perl=T)){
            date_filter<-paste0("?endPeriod=",sub("^<","",date_filter,perl=T))
          } else if (grepl("^[0-9\\-]{4,10}<$",date_filter,perl=T)){
            date_filter<-paste0("?startPeriod=",sub("<$","",date_filter,perl=T))
          } else if (grepl("^>[0-9\\-]{4,10}$",date_filter,perl=T)){
            date_filter<-paste0("?startPeriod=",sub("^>","",date_filter,perl=T))
          } else if (grepl("^[0-9\\-]{4,10}>$",date_filter,perl=T)){
            date_filter<-paste0("?endPeriod=",sub(">$","",date_filter,perl=T))
          } else if (grepl("^[0-9\\-]{4,10}$",date_filter,perl=T)){
            date_filter<-paste0("?startPeriod=",date_filter,"&endPeriod=",date_filter)
          } else{
            date_filter<-NULL
            warning("Could not parse date filter (it can be more than '<','>'; or not in [<>]yyyy[-mm][-dd][<>] format). The date filter is ignored.")
          }
        } else {
          date_filter<-NULL
          warning("Could not parse date filter (not in [<>]yyyy[-mm][-dd][<>] format). The date filter is ignored.")
        }  
      } else {
        if ((any(grepl("[^0-9\\-]",date_filter,perl=T)))){
          date_filter<-NULL
          warning("The date filter has invalid character (there are more more than 2 date periods: in this case only 0-9 and '-' can be used, no ':','<' or '>'). The date filter is ignored.")
        } else {
          if(all(sapply(date_filter,check_tf,tf=c("^[0-9]{4}$","^[0-9]{4}-[0-9]{2}$","^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))){
            date_filter<-sapply(date_filter,function(x) paste0("?startPeriod=",x,"&endPeriod=",x))
          } else {
            warning("Some of the date filter has invalid format (not in yyyy[-mm][-dd]). The wrong formatted date filter is ignored.")
            date_filter<-sapply(date_filter[sapply(date_filter,check_tf,tf=c("^[0-9]{4}$","^[0-9]{4}-[0-9]{2}$","^[0-9]{4}-[0-9]{2}-[0-9]{2}$"))],function(x) paste0("?startPeriod=",x,"&endPeriod=",x))
          }
        }
      }
    } 
    if (verbose){message(filters,"-",date_filter)}
    if (is.null(filters) & is.null(date_filter)){
      warning("None of the filter could be parsed. The whole dataset will be retrieved through bulk download.")
      restat<-get_eurostat_bulk(id,cache,update_cache,cache_dir,compress_file,stringsAsFactors,select_freq,keep_flags,...)
    } else {
      base_url<-eval(parse(text=paste0("cfg$QUERY_BASE_URL$'",rav,"'$ESTAT$data$'2.1'$data")))
      data_endpoint<-sub("\\/\\/(?=\\?)","/",paste0(base_url,"/",id,"/",filters,"/",date_filter),perl=T)
      restat<-data.table::rbindlist(lapply(data_endpoint, function(x) {
        if (verbose){
          message(x)
          try(rdat<-rsdmx::readSDMX(x),silent=T)
        } else {
          try(suppressWarnings(rdat<-rsdmx::readSDMX(x)),silent=T)
        }
        
        if (!is.null(rdat)){data.table::as.data.table(rdat)}else{NULL}
      }),fill=T)
      if (!is.null(restat)){
        if ((nrow(restat)==0)){
          restat<-NULL
          warning("There is no data with the given filter(s) or still too many observations after filtering. The bulk download can be used to download the whole dataset.")
        } else {
          if (length(unique(restat$FREQ))==1){
            drop<-c(drop,"FREQ")
          }
          if (keep_flags){
            if ("OBS_STATUS" %in% colnames(restat)){
              restat$OBS_STATUS[restat$OBS_STATUS=="na"]<-NA
              if (all(is.na(restat$OBS_STATUS)) & ("OBS_FLAG" %in% colnames(restat))){
                drop<-c(drop,"OBS_STATUS")
                data.table::setnames(restat,"OBS_FLAG","flags")
              } else {
                if ("OBS_FLAG" %in% colnames(restat)){
                  restat$flags<-paste0(restat$OBS_FLAG,restat$OBS_STATUS)
                  drop<-c(drop,"OBS_STATUS","OBS_FLAG")
                } else {
                  data.table::setnames(restat,"OBS_STATUS","flags")
                }
              }
            } else if ("OBS_FLAG" %in% colnames(restat)) {
              data.table::setnames(restat,"OBS_FLAG","flags")
            } else {
              restat$flags<-NA
            }
          } else {
            if ("OBS_STATUS" %in% colnames(restat)){drop<-c(drop,"OBS_STATUS")}
            if ("OBS_FLAG" %in% colnames(restat)){drop<-c(drop,"OBS_FLAG")}
          }    
          restat[,(drop):=NULL]
          data.table::setnames(restat,c("obsTime","obsValue"),c("time","values"))
          data.table::setnames(restat,colnames(restat),tolower(colnames(restat)))    
        }
      } else {
        warning("There is no data with the given filter(s) or still too many observations after filtering. The bulk download can be used to download the whole dataset.")
      }
    }
  }else{
    toc<-get_eurostat_toc()
    if ((cache)&(!update_cache)) {
      udate<-toc$lastUpdate[toc$code==id]
      restat<-get_eurostat_cache(paste0(id,"-",udate,"-",sum(keep_flags),"-",select_freq),cache_dir)
      if ((!is.null(restat))|(verbose)) {message("The data was loaded from cache.")}  
    }
    if ((!cache)|(is.null(restat))|(update_cache)){
        restat<-get_eurostat_bulk(id,cache,update_cache,cache_dir,compress_file,stringsAsFactors,select_freq,keep_flags,...)
    }
    if (cache){
      toc<-get_eurostat_toc()
      udate<-toc$lastUpdate[toc$code==id]
      pl<-put_eurostat_cache(restat,paste0(id,"-",udate,"-",sum(keep_flags),"-",select_freq),cache_dir,compress_file)
      if (verbose){message("The data was cached ",pl,".\n")}
    }
  }
  if (label & !is.null(restat)){
    dsd<-get_eurostat_dsd(id)
    cn<-colnames(restat)[!(colnames(restat) %in% c("time","values","flags"))]
    restat<-data.table::data.table(restat,stringsAsFactors=T) 
    sub_dsd<-dsd[dsd$code %in% unique(unlist(as.list(restat[,(cn),with=F])))]
    sub_dsd<-data.table::setorder(sub_dsd,concept,code)
    for (x in cn){
     levels(restat[[x]])<-sub_dsd$name[sub_dsd$concept==toupper(x)]
    }
    if (!stringsAsFactors){
      col_conv<-colnames(restat)[!(colnames(restat) %in% c("values"))]
      restat[,col_conv]<-restat[,lapply(.SD,as.character),.SDcols=col_conv]
    } 
  }
  restat
}


gen_ft<-function(x,ft){
  paste0(ft$code[ft$concept==x],collapse="+")
}

check_tf<-function(x,tf){
  any(sapply(tf,grepl,x=x,perl=T))
}



