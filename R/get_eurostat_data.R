#' @title Download/extract Eurostat Data
#' @description Download full or partial data set from \href{https://ec.europa.eu/eurostat/}{Eurostat} database.
#' @param id A code name for the dataset of interest.
#'        See \code{\link{search_eurostat_toc}} for details how to get an id.
#' @param filters a string or a character vector containing words to filter by the different concepts or geographical location.
#'        If filter applied only part of the dataset is downloaded through the API. The words can be  
#'        any words, Eurostat variable codes, and values available in the DSD \code{\link{search_eurostat_dsd}}. 
#'        The default is \code{NULL}, in this case the whole dataset is returned via the bulk download. To filter by time see \code{date_filter} below.
#'        If after filtering still the dataset has more observations than the limit per query via the API, then the bulk download is used to retrieve the data. 
#' @param exact_match a boolean with the default value \code{TRUE}, if the strings provided in \code{filters} shall be matched exactly as it is or as a pattern. 
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
#'        \code{options(restatapi_update=TRUE)}
#' @param cache_dir a path to a cache directory. The \code{NULL} (default) uses the memory as cache. 
#'        If the folder \code{cache_dir} directory does not exist it saves in the 'restatapi' directory 
#'        under the temporary directory from \code{tempdir()}. Directory can also be set with
#'        \code{option(restatapi_cache_dir=...)}.
#' @param compress_file a logical whether to compress the
#'        RDS-file in caching. Default is \code{TRUE}.
#' @param stringsAsFactors if \code{TRUE} (the default) the non-numeric columns are
#'        converted to factors. If the value \code{FALSE}
#'        they are returned as a characters.
#' @param keep_flags a logical whether the observation status (flags) - e.g. "confidential",
#'        "provisional", etc. - should be kept in a separate column or if they
#'        can be removed. Default is \code{FALSE}. For flag values see: 
#'        \url{http://ec.europa.eu/eurostat/data/database/information}.
#' @param check_toc a boolean whether to check the provided \code{id} in the Table of Contents (TOC) or not. The default value 
#'        \code{FALSE}, in this case the base URL for the download link is retrieved from the configuration file. 
#'        If the value is \code{TRUE} then the TOC is downloaded and the \code{id} is checked in it. If it found then the download link 
#'        is retrieved form the TOC.  
#' @param verbose A boolean with default \code{FALSE}, so detailed messages (for debugging) will not printed.
#'         Can be set also with \code{options(restatapi_verbose=TRUE)}
#' @param ... further arguments to the for \code{\link{search_eurostat_dsd}} function, e.g.: \code{ignore.case} or \code{name}. 
#'        If the \code{ignore.case} has the default value \code{FALSE}, then the strings provided in \code{filters} are matched as is, 
#'        otherwise the case of the letters is ignored. If the \code{name=FALSE} then the pattern(s) provided in the \code{filters}
#'        argument is only searched in the code column of the DSD, and the names of the codes will not be searched. 
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
#' The \code{id}, is a value from the \code{code} column of the table of contents (\code{\link{get_eurostat_toc}}), and can be searched 
#' for with the \code{\link{search_eurostat_toc}} function. The id value can be retrieved from the \href{http://ec.europa.eu/eurostat/data/database}{Eurostat database}
#'  as well. The Eurostat database gives codes in the Data Navigation Tree after every dataset in parenthesis.
#' 
#' Filtering can be done by the codes as described in the API documentation providing in the correct order and connecting with "." and "+". 
#' If we do not know the codes we can filter based on words or by the mix of the two putting in a vector like \code{c("AT$","Belgium","persons","Total")}. 
#' Be careful that the filter is case sensitive, if you do not know the code or label exactly you can use the option \code{ignore.case=TRUE} and \code{exact_match=FALSE}, 
#' but in this case the results may include unwanted elements as well. In the \code{filters} parameter regular expressions can be used as well. 
#' We do not have to worry about the correct order of the filter, it will be put in the correct place based on the DSD. 
#' 
#' 
#' The \code{date_filter} shall be a string in the format yyyy[-mm][-dd]. The month and the day part is optional, but if we use the years and we have monthly frequency then all the data for the given year is retrieved.
#' The string can be extended by adding the "<" or ">" to the beginning or to the end of the string. In this case the date filter is treated as range, and the date is used as a starting or end date. The data will include the observation of the start/end date.
#' A single date range can be defined as well by concatenating two dates with the ":", e.g. \code{"2016-08:2017-03-15"}. As seen in the exemple the dates can have different length: one defined only at year/month level, the other by day level. 
#' If a date range is defined with ":", it is not possible to use the "<" or ">" characters in the date filter.
#' If there are multiple dates which is not a continuous range, it can be put in vector in any order like \code{c("2016-08",2013:2015,"2017-07-01")}. In this case, as well, it is  not possible to use the  "<" or ">" characters.      
#'   
#' @return a data.table. One column for each dimension in the data,
#'         the time column for a time dimension, 
#'         the values column for numerical values and the flags column if the \code{keep_flags=TRUE}.
#'         Eurostat data does not include all missing values. The missing values are dropped if all dimensions are missing
#'         on particular time. 
#' @seealso \code{\link{search_eurostat_toc}},\code{\link{search_eurostat_dsd}}
#' @examples 
#' load_cfg()
#' eu<-get("cc",envir=.restatapi_env)
#' 
#' \dontshow{
#' if (parallel::detectCores()<=2){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }    
#' }
#' \donttest{
#' dt<-get_eurostat_data("NAMA_10_GDP")
#' dt<-get_eurostat_data("nama_10_gdp",update_cache=TRUE)
#' dt<-get_eurostat_data("nama_10_gdp",cache_dir="/tmp")
#' options(restatapi_update=FALSE)
#' options(restatapi_cache_dir=file.path(tempdir(),"restatapi"))
#' dt<-get_eurostat_data("avia_gonc",select_freq="A",cache=FALSE)
#' dt<-get_eurostat_data("agr_r_milkpr",date_filter=2008,keep_flags=TRUE)
#' dt<-get_eurostat_data("avia_par_me",
#'                       filters="BE$",
#'                       exact_match=FALSE,
#'                       date_filter=c(2016,"2017-03","2017-07-01"),
#'                       select_freq="Q",
#'                       label=TRUE,
#'                       name=FALSE)
#' dt<-get_eurostat_data("agr_r_milkpr",
#'                       filters=c("BE$","Hungary"),
#'                       date_filter="2007-06<",
#'                       keep_flags=TRUE)
#' dt<-get_eurostat_data("nama_10_a10_e",
#'                       filters=c("Annual","EU28","Belgium","AT","Total","EMP_DC","person"),
#'                       date_filter=c("2008",2002,2013:2018))
#' dt<-get_eurostat_data("vit_t3",
#'                       filters=c("EU28",eu$EA15,"HU$"),
#'                       date_filter=c("2015",2007))
#' dt<-get_eurostat_data("avia_par_me",
#'                       filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",
#'                       date_filter=c("2016-08","2017-07-01"),
#'                       select_freq="M")
#' }

get_eurostat_data <- function(id,
                         filters=NULL,
                         exact_match=TRUE,
                         date_filter=NULL,
                         label=FALSE,
                         select_freq=NULL,
                         cache=TRUE,
                         update_cache=FALSE,
                         cache_dir=NULL,
                         compress_file=TRUE,
                         stringsAsFactors=default.stringsAsFactors(),
                         keep_flags=FALSE,
                         check_toc=FALSE,
                         verbose=FALSE,...) {
  
  .datatable.aware=TRUE
  restat<-rdat<-drop<-concept<-code<-FREQ<-N<-NULL
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  update_cache<-update_cache|getOption("restatapi_update",FALSE)
  
  if (!(exists(".restatapi_env"))) {load_cfg()}
  
  cfg<-get("cfg",envir=.restatapi_env) 
  rav<-get("rav",envir=.restatapi_env)
  id<-tolower(id)
  ne<-dc<-TRUE
  
  if (check_toc){
    toc<-get_eurostat_toc(verbose=verbose)
    if (is.null(toc)){
      message("The TOC is missing. Could not get the download link.")
      dc<-FALSE
    } else {
      if (any(grepl(id,toc$code,ignore.case=TRUE))){
        udate<-toc$lastUpdate[grepl(id,toc$code,ignore.case=TRUE)]
        if (verbose) {message("data TOC rows: ",nrow(toc),"\nbulk url: ",toc$downloadLink.tsv[grepl(id,toc$code,ignore.case=TRUE)],"\ndata rowcount: ",toc$values[grepl(id,toc$code,ignore.case=TRUE)])}
      } else {
        message(paste0("'",id,"' is not in the table of contents. Please check if the 'id' is correctly spelled."))
        dc<-FALSE
      }
    }
  }else{
    udate<-Sys.Date()
  }
  
  if (dc){
    if(!is.null(select_freq)){
      append_sf<-FALSE
      if (is.null(filters)|(length(filters)>1)) {
        append_sf<-TRUE
      } else if (!is.null(filters)) {
        if (grepl("\\.",filters,perl=TRUE)){
          if (grepl("^\\.",filters,perl=TRUE)){
            filters<-paste0(select_freq,filters)
          } else{
            filters<-paste0(select_freq,"+",filters)
          }  
        } else {
          append_sf<-TRUE
        }
      }
      if (append_sf){
        if(select_freq=="A"){
          filters<-c(filters,"^Annual$","^A$")
        } else if (select_freq=="S") {
          filters<-c(filters,"^Semi-annual$","^S$")
        } else if (select_freq=="H") {
          filters<-c(filters,"^Half-year$","^H$")
        } else if (select_freq=="Q") {
          filters<-c(filters,"^Quarterly$","^Q$")
        } else if (select_freq=="M") {
          filters<-c(filters,"^Monthly$","^M$")
        } else if (select_freq=="W") {
          filters<-c(filters,"^Weekly$","^W$")
        } else if (select_freq=="D") {
          filters<-c(filters,"^Daily$","^D$")
        } else {
          select_freq<-NULL
          message("Incorrect frequency selected. It can be 'A', 'S', 'H', 'Q', 'M', 'W' or 'D'. The select_freq parameter will be ignored.")
        }
      }
    }
    if ((!is.null(filters))|(!is.null(date_filter))) {
      if (!is.null(filters)){
        dsd<-get_eurostat_dsd(id,verbose=verbose)
        if (is.null(dsd)){
          message("Could not download the DSD. The filter is ignored")
          filters_url<-NULL
        } else {
          dsdorder<-unique(dsd$concept)[1:(length(unique(dsd$concept))-2)]
          if (length(gregexpr("\\.",filters,perl=TRUE)[[1]])!=(length(dsdorder)-1)){
            ft<-do.call(rbind,lapply(filters,search_eurostat_dsd,dsd=dsd,exact_match=exact_match,...))
            if ((ncol(ft)>1)){
              ft<-unique(ft[ft$code!=FALSE,2:3])
              ft<-ft[order(match(ft$concept, dsdorder)),]
              filters_url<-paste0(sapply(dsdorder,gen_ft,ft),collapse=".")  
            } else {
              filters_url<-NULL
            }
          } else {filters_url<-filters}
        }
      } else {filters_url<-NULL}
      if (!is.null(date_filter)){
        date_filter<-as.character(date_filter)
        if (any(grepl("[^0-9\\-\\:<>]",date_filter,perl=TRUE))){
          date_filter<-NULL
          message("The date filter has invalid character (not 0-9, '-', '<', '>' or ':'). The date filter is ignored.")
        } else if (length(date_filter)==1)  {
          if (grepl(":",date_filter,perl=TRUE)){
            dates<-unlist(strsplit(date_filter,":"))
            if (length(dates)!=2){
              date_filter<-NULL
              message("Could not parse date range (more than one ':'). The date filter is ignored.")
            } else if (all(sapply(dates,check_tf,tf=c("^[0-9]{4}$","^[0-9]{4}-[0-9]{2}$","^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))){
              if ((as.numeric(gsub("-","",dates[2]))*(10^(8-nchar(as.numeric(gsub("-","",dates[2]))))))>(as.numeric(gsub("-","",dates[1]))*(10^(8-nchar(as.numeric(gsub("-","",dates[1]))))))){
                date_filter<-paste0("?startPeriod=",dates[1],"&endPeriod=",dates[2])
              } else {
                date_filter<-paste0("?startPeriod=",dates[2],"&endPeriod=",dates[1])
              }
            } else{
              date_filter<-NULL
              message("The date range has invalid character (only 0-9 and '-' can be used). The date filter is ignored.")
            }
          } else if (check_tf(date_filter,c("^[<>]?[0-9]{4}[<>]?$","^[<>]?[0-9]{4}-[0-9]{2}[<>]?$","^[<>]?[0-9]{4}-[0-9]{2}-[0-9]{2}[<>]?$"))){
            if (grepl("^<[0-9\\-]{4,10}$",date_filter,perl=TRUE)){
              date_filter<-paste0("?endPeriod=",sub("^<","",date_filter,perl=TRUE))
            } else if (grepl("^[0-9\\-]{4,10}<$",date_filter,perl=TRUE)){
              date_filter<-paste0("?startPeriod=",sub("<$","",date_filter,perl=TRUE))
            } else if (grepl("^>[0-9\\-]{4,10}$",date_filter,perl=TRUE)){
              date_filter<-paste0("?startPeriod=",sub("^>","",date_filter,perl=TRUE))
            } else if (grepl("^[0-9\\-]{4,10}>$",date_filter,perl=TRUE)){
              date_filter<-paste0("?endPeriod=",sub(">$","",date_filter,perl=TRUE))
            } else if (grepl("^[0-9\\-]{4,10}$",date_filter,perl=TRUE)){
              date_filter<-paste0("?startPeriod=",date_filter,"&endPeriod=",date_filter)
            } else{
              date_filter<-NULL
              message("Could not parse date filter (it can be more than '<','>'; or not in [<>]yyyy[-mm][-dd][<>] format). The date filter is ignored.")
            }
          } else {
            date_filter<-NULL
            message("Could not parse date filter (not in [<>]yyyy[-mm][-dd][<>] format). The date filter is ignored.")
          }  
        } else {
          if ((any(grepl("[^0-9\\-]",date_filter,perl=TRUE)))){
            date_filter<-NULL
            message("The date filter has invalid character (there are more more than 2 date periods: in this case only 0-9 and '-' can be used, no ':','<' or '>'). The date filter is ignored.")
          } else {
            if(all(sapply(date_filter,check_tf,tf=c("^[0-9]{4}$","^[0-9]{4}-[0-9]{2}$","^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))){
              date_filter<-sapply(date_filter,function(x) paste0("?startPeriod=",x,"&endPeriod=",x))
            } else {
              message("Some of the date filter has invalid format (not in yyyy[-mm][-dd]). The wrong formatted date filter is ignored.")
              date_filter<-sapply(date_filter[sapply(date_filter,check_tf,tf=c("^[0-9]{4}$","^[0-9]{4}-[0-9]{2}$","^[0-9]{4}-[0-9]{2}-[0-9]{2}$"))],function(x) paste0("?startPeriod=",x,"&endPeriod=",x))
            }
          }
        }
      } 
      if (verbose){message(filters_url,"-",date_filter)}
      if (is.null(filters_url)&(is.null(date_filter))){
        message("None of the filter could be applied. The whole dataset will be retrieved through bulk download.")
        restat<-get_eurostat_bulk(id,cache,update_cache,cache_dir,compress_file,stringsAsFactors,select_freq,keep_flags,verbose)
      } else {
        base_url<-eval(parse(text=paste0("cfg$QUERY_BASE_URL$'",rav,"'$ESTAT$data$'2.1'$data")))
        data_endpoint<-sub("\\/\\/(?=\\?)","/",paste0(base_url,"/",id,"/",filters_url,"/",date_filter),perl=TRUE)
        if (verbose) {
          restat<-data.table::rbindlist(lapply(data_endpoint, function(x) {
            message(x)
            temp <- tempfile()
            tryCatch({utils::download.file(x,temp,get("dmethod",envir=.restatapi_env))},
                     error = function(e) {
                       message("Error by the download the xml file:",'\n',paste(unlist(e),collapse="\n"))
                       ne<-FALSE
                     },
                     warning = function(w) {
                       message("Warning by the download the xml file:",'\n',paste(unlist(w),collapse="\n"))
                     })
            if(ne){
              xml_leafs<-xml2::xml_find_all(xml2::read_xml(temp),".//generic:Series")
              if (Sys.info()[['sysname']]=='Windows'){
                xml_leafs<-as.character(xml_leafs)
                cl<-parallel::makeCluster(min(2,getOption("restatapi_cores",1L)))
                parallel::clusterEvalQ(cl,require(xml2))
                parallel::clusterExport(cl,c("extract_data"))
                rdat<-data.table::rbindlist(parallel::parLapply(cl,xml_leafs,extract_data,keep_flags=keep_flags,stringsAsFactors=stringsAsFactors,bulk=FALSE))              
                parallel::stopCluster(cl)
              }else{
                rdat<-data.table::rbindlist(parallel::mclapply(xml_leafs,extract_data,keep_flags=keep_flags,stringsAsFactors=stringsAsFactors,bulk=FALSE,mc.cores=getOption("restatapi_cores",1L)))                                  
              }
            }
            if (!is.null(rdat)){data.table::as.data.table(rdat)}
          }),fill=TRUE)
        } else {
          restat<-data.table::rbindlist(lapply(data_endpoint, function(x) {
            temp <- tempfile()
            tryCatch({utils::download.file(x,temp,get("dmethod",envir=.restatapi_env),quiet=TRUE)},
                     error = function(e) {ne<-FALSE},
                     warning = function(w) {})
            if(ne){
              xml_leafs<-xml2::xml_find_all(xml2::read_xml(temp),".//generic:Series")
              if (Sys.info()[['sysname']]=='Windows'){
                xml_leafs<-as.character(xml_leafs)
                cl<-parallel::makeCluster(min(2,getOption("restatapi_cores",1L)))
                parallel::clusterEvalQ(cl,require(xml2))
                parallel::clusterExport(cl,c("extract_data"))
                rdat<-data.table::rbindlist(parallel::parLapply(cl,xml_leafs,extract_data,keep_flags=keep_flags,stringsAsFactors=stringsAsFactors,bulk=FALSE))              
                parallel::stopCluster(cl)
              }else{
                rdat<-data.table::rbindlist(parallel::mclapply(xml_leafs,extract_data,keep_flags=keep_flags,stringsAsFactors=stringsAsFactors,bulk=FALSE,mc.cores=getOption("restatapi_cores",1L)))                                  
              }
            }
            if (!is.null(rdat)){data.table::as.data.table(rdat)}
          }),fill=TRUE)
        }
        if (!is.null(restat)){
          if ((nrow(restat)==0)){
            message("There is no data with the given filter(s) or still too many observations after filtering. The bulk download is used to download the whole dataset.")
            restat<-get_eurostat_bulk(id,cache,update_cache,cache_dir,compress_file,stringsAsFactors,select_freq,keep_flags,check_toc,verbose)
          } else {
            if (verbose) {message("restat - nrow:",nrow(restat),";ncol:",ncol(restat),";colnames:",paste(colnames(restat),collapse="/"))}
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
            if(!is.null(drop)) {restat[,(drop):=NULL]}
            data.table::setnames(restat,c("obsTime","obsValue"),c("time","values"))
            data.table::setnames(restat,colnames(restat),tolower(colnames(restat)))
            restat$time<-gsub('[MD]',"-",restat$time)
            restat$time<-gsub('([0-9]{4})Q',"\\1-Q",restat$time,perl=TRUE)
            restat<-data.table::data.table(restat,key=names(restat),stringsAsFactors=stringsAsFactors)
            restat<-unique(restat)
            restat[order("time"),]
          }
        } 
      }
    }else{
      if ((cache)&(!update_cache)) {
        restat<-get_eurostat_cache(paste0("b_",id,"-",udate,"-",sum(keep_flags),sub("-$","",paste0("-",select_freq))),cache_dir,verbose=verbose)
        if (!is.null(restat)){
          drop=c("FREQ","TIME_FORMAT")
          if ((is.null(select_freq))){
            if (length(unique(restat$FREQ))>1){
              st<-data.table::setorder(restat[,.N,by=FREQ],-N)[1,1]
              if (stringsAsFactors){select_freq<-as.character(levels(st$FREQ)[st$FREQ[1]])}else{as.character(st$FREQ)}
              warning("There are multiple frequencies in the dataset. The '", select_freq, "' is selected as it is the most common frequency.")
            } 
          } else {
            restat<-restat[FREQ==select_freq]
          }
          if ("OBS_VALUE" %in% colnames(restat)) {
            if (keep_flags){
              data.table::setnames(restat,"OBS_STATUS","flags")
            } else {
              if ("OBS_STATUS" %in% colnames(restat)) {drop<-c(drop,"OBS_STATUS")}    
            }
            restat[,(drop):=NULL]
            data.table::setnames(restat,c("TIME_PERIOD","OBS_VALUE"),c("time","values"))
          } 
          restat$time<-gsub('[MD]',"-",restat$time)
          restat$time<-gsub('([0-9]{4})Q',"\\1-Q",restat$time,perl=TRUE)
          if (("flags" %in% colnames(restat))&(!keep_flags)){restat[,("flags"):=NULL]}
          if (is.factor(restat$values)){restat$values<-as.numeric(levels(restat$values))[restat$values]} else{restat$values<-as.numeric(restat$values)}
          if (verbose) {message("The data was loaded from cache.")} 
          if (any(sapply(restat,is.factor))&(!stringsAsFactors)) {
            col_conv<-colnames(restat)[!(colnames(restat) %in% c("values"))]
            restat[,col_conv]<-restat[,lapply(.SD,as.character),.SDcols=col_conv]
          }
          if (!any(sapply(restat,is.factor))&(stringsAsFactors)) {
            restat<-data.table::data.table(restat,stringsAsFactors=TRUE)
          }
        }
      }
      if ((!cache)|(is.null(restat))|(update_cache)){
        restat<-get_eurostat_bulk(id,cache,update_cache,cache_dir,compress_file,stringsAsFactors,select_freq,keep_flags,verbose)
      }
      if (cache&(!is.null(restat))){
        oname<-paste0("b_",id,"-",udate,"-",sum(keep_flags),sub("-$","",paste0("-",select_freq),perl=TRUE))
        pl<-put_eurostat_cache(restat,oname,update_cache,cache_dir,compress_file)
        if (verbose){message("The data was cached ",pl,".\n")}
      }
    }
    if (label & !is.null(restat)){
      if (verbose) {message("restat - nrow:",nrow(restat),";ncol:",ncol(restat))}
      dsd<-get_eurostat_dsd(id,verbose=verbose)
      if (!is.null(dsd)){
        if (verbose) {message("dsd - nrow:",nrow(dsd),";ncol:",ncol(dsd))}
        cn<-colnames(restat)[!(colnames(restat) %in% c("time","values","flags"))]
        restat<-data.table::data.table(restat,stringsAsFactors=TRUE) 
        if (verbose) {message("data - nrow:",nrow(restat),";ncol:",ncol(restat),";colnames:",paste(cn,collapse="/"))}
        sub_dsd<-dsd[dsd$code %in% as.character(levels(unique(unlist(as.list(restat[,(cn),with=FALSE]))))),]
        sub_dsd<-data.table::setorder(sub_dsd,concept,code)
        for (x in cn){
          levels(restat[[x]])<-sub_dsd$name[sub_dsd$concept==toupper(x)]
        }
        if (!stringsAsFactors){
          col_conv<-colnames(restat)[!(colnames(restat) %in% c("values"))]
          restat[,col_conv]<-restat[,lapply(.SD,as.character),.SDcols=col_conv]
        }  
      } else {
        message("Could not download the DSD. No label is applied.")
      }
    }  
  }
  return(restat)
}


gen_ft<-function(x,ft){
  paste0(ft$code[ft$concept==x],collapse="+")
}
check_tf<-function(x,tf){
  any(sapply(tf,grepl,x=x,perl=TRUE))
}



