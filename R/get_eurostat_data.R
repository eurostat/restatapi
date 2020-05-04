#' @title Download/extract Eurostat Data
#' @description Download full or partial data set from \href{https://ec.europa.eu/eurostat/}{Eurostat} database.
#' @param id A code name for the dataset of interest.
#'        See \code{\link{search_eurostat_toc}} for details how to get an id.
#' @param filters a string, a character vector or named list containing words to filter by the different concepts or geographical location.
#'        If filter applied only part of the dataset is downloaded through the API. The words can be  
#'        any word, Eurostat variable code, and value which are in the DSD \code{\link{search_eurostat_dsd}}. 
#'        If a named list is used, then the name of the list elements should be the concepts from the DSD and the provided values will be used to filter the dataset for the given concept.
#'        The default is \code{NULL}, in this case the whole dataset is returned via the bulk download. To filter by time see \code{date_filter} below.
#'        If after filtering still the dataset has more observations than the limit per query via the API, then the raw download is used to retrieve the whole dataset and apply the filter on the local computer. This option can be disabled with the \code{local_filter=FALSE} parameter. 
#' @param exact_match a boolean with the default value \code{TRUE}, if the strings provided in \code{filters} shall be matched exactly as it is or as a pattern. 
#' @param date_filter a vector which can be numeric or character containing dates to filter the dataset. If date is defined as character string it should follow the format yyyy[-mm][-dd], where the month and the day part is optional. 
#'        If date filter applied only part of the dataset is downloaded through the API. 
#'        The default is \code{NULL}, in this case the whole dataset is returned via the bulk download.
#'        If after filtering still the dataset has more observations than the limit per query via the API, then the raw download is used to retrieve the data and apply the filter on the local computer. 
#'        This option can be disabled with the \code{local_filter=FALSE} parameter. 
#' @param label a boolean with the default \code{FALSE}. If it is \code{TRUE} then the code values are replaced by the name from the Data Structure Definition (DSD) \code{\link{get_eurostat_dsd}}.
#'         For example instead of "D1110A", "Raw cows' milk from farmtype" is used or "HU32" is replaced by "Észak-Alföld".
#' @param select_freq a character symbol for a time frequency when a dataset has multiple time
#'        frequencies. Possible values are:
#'    	  A = annual, S = semi-annual, H = half-year, Q = quarterly, M = monthly, W = weekly, D = daily. 
#'    	  The default is \code{NULL} as most datasets have just one time
#'        frequency and in case there are multiple frequencies, then only the most common frequency kept.
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
#'        converted to factors. If the value \code{FALSE} they are returned as characters.
#' @param keep_flags a logical whether the observation status (flags) - e.g. "confidential",
#'        "provisional", etc. - should be kept in a separate column or if they
#'        can be removed. Default is \code{FALSE}. For flag values see: 
#'        \url{http://ec.europa.eu/eurostat/data/database/information}.
#' @param cflags a logical whether the missing observations with flag 'c' - "confidential"
#'        should be kept or not. Default is \code{FALSE}, in this case these observations dropped from the dataset. If this parameter 
#'        \code{TRUE} then the flags are kept and the parameter provided in \code{keep_flags} is not taken into account.
#' @param check_toc a boolean whether to check the provided \code{id} in the Table of Contents (TOC) or not. The default value 
#'        \code{FALSE}, in this case the base URL for the download link is retrieved from the configuration file. 
#'        If the value is \code{TRUE} then the TOC is downloaded and the \code{id} is checked in it. If it found then the download link 
#'        is retrieved form the TOC.  
#' @param local_filter a boolean whether do the filtering on the local computer or not in case after filtering still the dataset has more observations 
#'        than the limit per query via the API would allow to download. The default is \code{TRUE}, in this case if the response footer contains information 
#'        that the result cannot be downloaded becuase it is too large, then the whole raw dataset is downloaded and filtered on the local computer.  
#' @param force_local_filter a boolean with the default value \code{FALSE}. In case, if there are existing filter conditions, then it will do the filtering on the local 
#'        computer and not requesting through the REST API. It can be useful, if the values are not numeric as these are provided as NaN (Not a Number) through the REST API, 
#'        but it is fully listed in the raw dataset. 
#' @param verbose A boolean with default \code{FALSE}, so detailed messages (for debugging) will not printed.
#'         Can be set also with \code{options(restatapi_verbose=TRUE)}
#' @param ... further arguments to the for \code{\link{search_eurostat_dsd}} function, e.g.: \code{ignore.case} or \code{name}. 
#'        The \code{ignore.case} has the default value \code{FALSE}, then the strings provided in \code{filters} are matched as is, 
#'        otherwise the case of the letters is ignored. If the \code{name=FALSE} then the pattern(s) provided in the \code{filters}
#'        argument is only searched in the code column of the DSD, and the names of the codes will not be searched. 
#' @export
#' 
#' @details Data sets are downloaded from the Eurostat Web Services 
#' \href{https://ec.europa.eu/eurostat/web/sdmx-web-services}{SDMX API} if there is a filter otherwise the 
#' \href{http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing}{the Eurostat bulk download facility} is used.
#' If only the table \code{id} is given, the whole table is downloaded from the
#' bulk download facility. If also \code{filters} or \code{date_filter} is defined then the SDMX REST API is
#' used. In case after filtering the dataset has more rows than the limitation of the SDMX REST API (1 million values at one time) then the bulk download is used to retrieve the whole dataset .
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
#' A single date range can be defined as well by concatenating two dates with the ":", e.g. \code{"2016-08:2017-03-15"}. As seen in the example the dates can have different length: one defined only at year/month level, the other by day level. 
#' If a date range is defined with ":", it is not possible to use the "<" or ">" characters in the date filter.
#' If there are multiple dates which is not a continuous range, it can be put in vector in any order like \code{c("2016-08",2013:2015,"2017-07-01")}. In this case, as well, it is  not possible to use the  "<" or ">" characters.      
#'   
#' @return a data.table.a data.table with the following columns: #'  \tabular{ll}{
#'      \code{freq} \tab A column for the frequency of the data in case there are multiple frequencies, for single frequency this columns is dropped from the data table \cr
#'      dimension names \tab One column for each dimension in the data \cr
#'      \code{time} \tab A column for the time dimension\cr
#'      \code{values} \tab A column for numerical values\cr
#'      \code{flags} \tab A column for flags if the \code{keep_flags=TRUE} or \code{cflags=TRUE} otherwise this column is not included in the data table
#'    }
#'         The data.table does not include all missing values. The missing values are dropped if the value and flag are missing
#'         on a particular time. 
#' @seealso \code{\link{search_eurostat_toc}}, \code{\link{search_eurostat_dsd}}
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
#' dt<-get_eurostat_data("htec_cis3",update_cache=TRUE,check_toc=TRUE)
#' dt<-get_eurostat_data("agr_r_milkpr",cache_dir="/tmp",cflags=TRUE)
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
#' dt<-get_eurostat_data("htec_cis3",
#'                        filters="lu",
#'                        ignore.case=TRUE) 
#' dt<-get_eurostat_data("bop_its6_det",
#'                        filters=list(bop_item="SC",
#'                                     currency="MIO_EUR",
#'                                     partner="EXT_EU28",
#'                                     geo=c("EU28","HU"),
#'                                     stk_flow="BAL"),
#'                        date_filter="2010:2012",
#'                        select_freq="A",
#'                        label=TRUE,
#'                        name=FALSE)         
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
                         stringsAsFactors=TRUE,
                         keep_flags=FALSE,
                         cflags=FALSE,
                         check_toc=FALSE,
                         local_filter=TRUE,
                         force_local_filter=FALSE,
                         verbose=FALSE,...) {
  
  .datatable.aware=TRUE
  restat<-rdat<-drop<-concept<-code<-FREQ<-N<-flags<-NULL
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  update_cache<-update_cache|getOption("restatapi_update",FALSE)
  tbc<-cr<-TRUE # to be continued for the next steps  / cache result data.table 
  if(cflags){keep_flags<-cflags}
  
  if (!(exists(".restatapi_env"))) {load_cfg()}
  if (getOption("restatapi_dev",FALSE)){
    tryCatch(
      {logstr<-paste(utils::packageVersion("restatapi"),paste(paste(names(match.call()),match.call(),sep="%3D")[2:length(match.call())],collapse="\t"),sep="\t")
      utils::download.file(paste0("http://restatapi.azurewebsites.net/restatapi.php?params=",gsub("\\s","%20",gsub("\\t","%09",utils::URLencode(logstr,TRUE)))),"resp",quiet=(!verbose))
      unlink("resp",force=TRUE)},
      error=function(e){},
      warning=function(w){}
    )  
  }
  
  
  cfg<-get("cfg",envir=.restatapi_env) 
  rav<-get("rav",envir=.restatapi_env)
  if (!is.null(id)){id<-tolower(id)} else {
    tbc<-FALSE
    check_toc<-FALSE
    message("The dataset 'id' is missing.")
  }
  
  if (check_toc){
    toc<-get_eurostat_toc(verbose=verbose)
    if (is.null(toc)){
      message("The TOC is missing. Could not get the download link.")
      tbc<-FALSE
    } else {
      if (any(grepl(id,toc$code,ignore.case=TRUE))){
        udate<-toc$lastUpdate[grepl(id,toc$code,ignore.case=TRUE)]
        if (verbose) {message("data TOC rows: ",nrow(toc),"\nbulk url: ",toc$downloadLink.tsv[grepl(id,toc$code,ignore.case=TRUE)],"\ndata rowcount: ",toc$values[grepl(id,toc$code,ignore.case=TRUE)])}
      } else {
        message(paste0("'",id,"' is not in the table of contents. Please check if the 'id' is correctly spelled."))
        tbc<-FALSE
      }
    }
  }else{
    udate<-format(Sys.Date(),"%Y.%m.%d")
  }
  
  if (tbc){
    if(!is.null(select_freq)){
      append_sf<-FALSE
      if (is.null(filters)|(length(filters)>1)) {
        append_sf<-TRUE  # there is already filters defined and the select_freq is appended to the the filters
      } else if (!is.null(filters)) {  
        if (grepl("\\.",filters,perl=TRUE)){ #filter is given as a string for the REST API
          if (grepl("^\\.",filters,perl=TRUE)){ # no FREQ value is given in the filter
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
            ft<-create_filter_table(filters,FALSE,dsd,exact_match,...)
            # if (is.null(names(filters))){
            #   ft<-data.table::rbindlist(lapply(filters,search_eurostat_dsd,dsd=dsd,exact_match=exact_match,...))
            # } else{
            #   concepts<-names(filters)
            #   ft<-data.table::rbindlist(lapply(1:length(filters),function (x,f,d){
            #     do.call(rbind,lapply(unlist(f[x]),search_eurostat_dsd,dsd=d[d$concept==toupper(concepts[x]),],exact_match=exact_match,...))
            #   },f=filters,d=dsd))
            # }
            if (nrow(ft)>0){
              ft<-unique(ft[ft$code!=FALSE,2:3])
              ft<-ft[order(match(ft$concept, dsdorder)),]
              filters_url<-paste0(sapply(dsdorder,gen_ft,ft),collapse=".")  
            } else {
              filters_url<-NULL
            }
          } else {filters_url<-filters}
        }
      } else {
        ft<-NULL
        filters_url<-NULL
      }
      if (!is.null(date_filter)){
        if (verbose) {message("date_filter: ",match.call()$date_filter)}
        dft<-create_filter_table(match.call()$date_filter,TRUE,verbose=verbose)
        if (verbose) {message("date_filter: ",paste(date_filter,collapse=", ")," nrow dft: ",nrow(dft))}
        # df<-as.character(substitute(date_filter))
        # if (df[1]=="c"){
        #   df<-df[2:length(df)]
        # } else {
        #   df<-as.character(parse(text=deparse(date_filter)))
        # }
        # if (any(grepl("[^0-9\\-\\:<>]",df,perl=TRUE))){
        #   df<-gsub("[^0-9\\-\\:<>]","",df,perl=TRUE)
        #   df<-df[df!=""]
        #   message("The date filter had invalid character (not 0-9, '-', '<', '>' or ':'). Those charcters removed from the date filter.")
        # } 
        # if (verbose){message(paste(df,collapse=", ")," date filter length: ",length(df),", nchar date_filter: ",paste(nchar(df),collapse=","))}
        # dft<-data.table::rbindlist(lapply(df, function(sdf) {
        #     if (nchar(gsub("[^:<>]","",sdf,perl=TRUE))>1){
        #       res<-NULL
        #       if(verbose){message(paste0("Could not parse date filter: '",sdf,"'. This date filter is ignored."))}
        #     } else {
        #     if (grepl(":",sdf,perl=TRUE)){
        #       dates<-unlist(strsplit(sdf,":"))
        #       if (all(sapply(dates,check_tf,tf=c("^[0-9]{4}$","^[0-9]{4}-[0-1][0-9]$","^[0-9]{4}-[0-1][0-9]-[0-3][0-9]$")))){
        #         res<-list(sd=min(dates),ed=max(dates))
        #       } else{
        #         res<-NULL
        #         if(verbose){message(paste0("Could not parse date filter: '",paste0(dates,collapse=":"),"' (at least one date not in yyyy[-mm][-dd] format). The date filter is ignored."))}
        #       }
        #     } else if (grepl("<|>",sdf,perl=TRUE)){
        #       if(check_tf(gsub("<|>","",sdf,perl=TRUE),c("^[0-9]{4}$","^[0-9]{4}-[0-1][0-9]$","^[0-9]{4}-[0-1][0-9]-[0-3][0-9]$"))){
        #         if (grepl("^<|>$",sdf,perl=TRUE)){
        #           res<-list(sd=0,ed=gsub("<|>","",sdf,perl=TRUE))  
        #         } else if (grepl("^>|<$",sdf,perl=TRUE)){
        #           res<-list(sd=gsub("<|>","",sdf,perl=TRUE),ed=Inf)
        #         } else {
        #           res<-NULL
        #           if (verbose) {message(paste0("Could not parse date filter: '", sdf,"' not in [<>]yyyy[-mm][-dd][<>] format). The date filter is ignored."))}
        #         }
        #       } else {
        #         res<-NULL
        #         if(verbose){message(paste0("Could not parse date filter: '",sdf,"' (not in yyyy[-mm][-dd] format). The date filter is ignored."))}
        #       }
        #     } else{
        #       if(check_tf(sdf,c("^[0-9]{4}$","^[0-9]{4}-[0-1][0-9]$","^[0-9]{4}-[0-1][0-9]-[0-3][0-9]$"))){
        #         res<-list(sd=sdf,ed=sdf)  
        #       } else {
        #         res<-NULL
        #         if (verbose) {message(paste0("Could not parse date filter: '",sdf,"' not in [<>]yyyy[-mm][-dd][<>] format). The date filter is ignored."))}
        #       }
        #     }  
        #   }
        #     return(res)
        #   }),fill=TRUE)
        if(!is.null(dft)){
          if(nrow(dft)>0){
            # dft<-dft[order(sd,ed)]
            # dft[, list(sd=min(sd), ed=max(ed)),by=list(group=cumsum(c(1, utils::tail(sd, -1) > utils::head(ed, -1))))]
            date_filter<-apply(dft,1,function (x) {paste0("?",if(x[1]!=0){paste0("startPeriod=",x[1])},if(x[1]!=0&x[2]!=Inf){paste0("&endPeriod=",x[2])},if(x[1]==0&x[2]!=Inf){paste0("endPeriod=",x[2])})})  
          } else {
            date_filter<-NULL
          }  
        } else{
          date_filter<-NULL
        }
      }else{dft<-NULL}
      if (verbose){message(filters_url,"-",date_filter)}
      if (is.null(filters_url)&(is.null(date_filter))){
        message("None of the filter could be applied. The whole dataset will be retrieved through bulk download.")
        if (verbose) {message("cflags:",cflags)}
        restat<-get_eurostat_bulk(id,cache,update_cache,cache_dir,compress_file,stringsAsFactors,select_freq,keep_flags,cflags,check_toc,verbose=verbose)
      } else  if (force_local_filter){
        message("Forcing to apply filter locally. The whole dataset is downloaded through the raw download and the filters are applied locally.")
        restat_raw<-get_eurostat_raw(id,"txt",cache,update_cache,cache_dir,compress_file,stringsAsFactors,keep_flags,check_toc,verbose)
        if (verbose) {print(ft)}
        if (!is.null(dft)){
          if (nrow(ft)>0){restat_raw<-filter_raw_data(restat_raw,ft)[]}
        }
        if (verbose) {print(dft)}
        if (!is.null(dft)){
          if (nrow(dft)>0){restat_raw<-filter_raw_data(restat_raw,dft,TRUE)[]}
        }
        cr<-FALSE
        restat<-restat_raw[]
      } else {
        base_url<-eval(parse(text=paste0("cfg$QUERY_BASE_URL$'",rav,"'$ESTAT$data$'2.1'$data")))
        data_endpoint<-sub("\\/\\/(?=\\?)","/",paste0(base_url,"/",id,"/",filters_url,"/",date_filter),perl=TRUE)
        options(code_opt=NULL)
        restat<-data.table::rbindlist(lapply(data_endpoint, function(x) {
            if (verbose) {message(x)}
            temp <- tempfile()
            tryCatch({utils::download.file(x,temp,get("dmethod",envir=.restatapi_env),quiet=!verbose)},
                     error = function(e) {
                       if (verbose) {message("Error by the download the xml file:",'\n',paste(unlist(e),collapse="\n"))}
                       tbc<-FALSE
                     },
                     warning = function(w) {
                       if(verbose){message("Warning by the download the xml file:",'\n',paste(unlist(w),collapse="\n"))}
                       tbc<-FALSE
                     })
            if (length(temp)==0) {tbc<-FALSE}
            if (tbc){
              xml_foot<-xml2::xml_find_all(xml2::read_xml(temp),".//footer:Message")
              if (length(xml_foot)>0){
                code<-xml2::xml_attr(xml_foot,"code")
                severity<-xml2::xml_attr(xml_foot,"severity")
                fmsg<-xml2::xml_text(xml2::xml_children(xml_foot))
                if (verbose){message(x,"\n",code," - ",severity,"\n",paste(fmsg,collapse="\n"))}
                code<-c(getOption("code_opt",NULL),code)
                options(code_opt=code)
                tbc<-FALSE
              }  
            }
            if(tbc){
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
            if (!is.null(rdat)){data.table::as.data.table(rdat,stringsAsFactors=stringsAsFactors)}  
        }),fill=TRUE)
        if (!is.null(restat)){
          if ((nrow(restat)==0)) {
            if (all(getOption("code_opt",NULL)==500)){
              message("500 - No data with the given filter(s)")
              restat<-NULL
            } else {
              if (local_filter){
                message("No data retrieved for the given filter(s), because the results are too big to download immediately through the REST API. The whole dataset is downloaded through the raw download and the filters are applied locally.")
                restat_raw<-get_eurostat_raw(id,"txt",cache,update_cache,cache_dir,compress_file,stringsAsFactors,keep_flags,check_toc,verbose)
                if (verbose) {print(ft)}
                if (!is.null(ft)){
                  if (nrow(ft)>0){restat_raw<-filter_raw_data(restat_raw,ft)}
                }
                if (verbose) {print(dft)}
                if (!is.null(dft)){
                  if (nrow(dft)>0){restat_raw<-filter_raw_data(restat_raw,dft,TRUE)}
                }
                cr<-FALSE
                restat<-restat_raw
              } else {
                message("No data retrieved for the given filter(s), because the results are too big to download immediately through the REST API. You may want to download the whole dataset and apply the filter(s) locally.")
              }
            }
          } else if (any(getOption("code_opt",NULL)==413)){
            if (local_filter){
              message("One or some of the filter(s) resulted too large datatset to download through the REST API. The whole dataset is downloaded through the raw download and the filters are applied locally.")
              restat_raw<-get_eurostat_raw(id,"txt",cache,update_cache,cache_dir,compress_file,stringsAsFactors,keep_flags,check_toc,verbose)
              if (verbose) {print(ft)}
              if (nrow(ft)>0){restat_raw<-filter_raw_data(restat_raw,ft)}
              if (verbose) {print(dft)}
              if (nrow(dft)>0){restat_raw<-filter_raw_data(restat_raw,dft,TRUE)}
              cr<-FALSE
              restat<-restat_raw
            } else {
              message("One or some of the filter(s) resulted too large datatset to download through the REST API.You may want to download the whole dataset and apply the filter(s) locally.")
            }
          } else {
            cr<-FALSE # do not cahce filtered data only bulk datasets
            if (verbose) {message("restat - nrow:",nrow(restat),";ncol:",ncol(restat),";colnames:",paste(colnames(restat),collapse="/"))}
            if (length(unique(restat$FREQ))==1){
              drop<-c(drop,"FREQ")
            }
            if (keep_flags){
              if ("OBS_STATUS" %in% colnames(restat)){
                restat$OBS_STATUS[restat$OBS_STATUS=="na"]<-""
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
            dsd<-get_eurostat_dsd(id,verbose=verbose)
            dsdorder<-tolower(unique(dsd$concept)[1:length(unique(dsd$concept))])
            co<-dsdorder[dsdorder %in% colnames(restat)]
            data.table::setcolorder(restat,co)  
          }
        } 
      }
    }else{
      if ((cache)&(!update_cache)) {
        nm<-paste0("b_",id,"-",udate,"-",sum(keep_flags),"-",sum(cflags),sub("-$","",paste0("-",select_freq)))
        if (verbose) {message("Trying to get from cache: ",nm)}
        restat<-data.table::copy(get_eurostat_cache(nm,cache_dir,verbose=verbose))
        if (!is.null(restat)){
          if (verbose) {message("The data was loaded from cache.\ncached restat - nrow:",nrow(restat),";ncol:",ncol(restat),";colnames:",paste(colnames(restat),collapse="/"))}
          if ("FREQ" %in% colnames(restat)) {
            drop=c("FREQ")
            if ((is.null(select_freq))){
              if (length(unique(restat$FREQ))>1){
                st<-data.table::setorder(restat[,.N,by=FREQ],-N)[1,1]
                if (stringsAsFactors){select_freq<-as.character(levels(st$FREQ)[st$FREQ[1]])}else{as.character(st$FREQ)}
                warning("There are multiple frequencies in the dataset. The '", select_freq, "' is selected as it is the most common frequency.")
              } 
            } else {
              restat<-restat[restat$FREQ==select_freq]
            }
          }
          if ("TIME_FORMAT" %in% colnames(restat)) {drop<-c(drop,"TIME_FORMAT")} 
          if ("OBS_STATUS" %in% colnames(restat)) {data.table::setnames(restat,"OBS_STATUS","flags")}
          if ("TIME_PERIOD" %in% colnames(restat)) {data.table::setnames(restat,"TIME_PERIOD","time")}
          if ("OBS_VALUE" %in% colnames(restat)) {data.table::setnames(restat,"OBS_VALUE","values")}
          if(!is.null(drop)){restat[,(drop):=NULL]}
        }
        if (verbose) {message("processed restat - nrow:",nrow(restat),";ncol:",ncol(restat),";colnames:",paste(colnames(restat),collapse="/"))}
      }
      if ((!cache)|(is.null(restat))|(update_cache)){
        if (verbose) {message("cflags:",cflags)}
        restat<-get_eurostat_bulk(id,cache,update_cache,cache_dir,compress_file,stringsAsFactors,select_freq,keep_flags,cflags,check_toc,verbose=verbose)
      }
    }
    if (!is.null(restat)){
      if ("freq" %in% colnames(restat)){
        if (length(unique(restat$freq))==1){restat[,"freq":=NULL] }
      }
      restat<-unique(restat)[]
      restat$time<-gsub('[MD]',"-",restat$time)
      restat$time<-gsub('([0-9]{4})Q',"\\1-Q",restat$time,perl=TRUE)
      if ("flags" %in% colnames(restat)){
        restat<-restat[!(is.na(restat$values)&(is.na(restat$flags)|restat$flags==""))]
        if (keep_flags) {
          restat$flags<-as.character(restat$flags)
          restat[is.na(flags),flags:=""]
          if (!cflags) {restat<-restat[restat$flags!="c"]}
        } else{
          restat[,"flags":=NULL]
        }
      } else {
        restat<-restat[!(is.na(restat$values))]
      }  
      restat<-data.table::data.table(restat,key=names(restat),stringsAsFactors=stringsAsFactors)
      if (any(sapply(restat,is.factor))&(!stringsAsFactors)) {
        col_conv<-colnames(restat)[!(colnames(restat) %in% c("values"))]
        restat[,col_conv]<-restat[,lapply(.SD,as.character),.SDcols=col_conv]
      }
      if (!any(sapply(restat,is.factor))&(stringsAsFactors)) {
        restat<-data.table::data.table(restat,stringsAsFactors=stringsAsFactors)
      }
      if (is.factor(restat$values)){
        if (any(grepl('\\d+\\:\\d+',restat$values))){
          restat$values<-as.character(levels(restat$values))[restat$values]
          restat$values[grepl('^\\:$',restat$values)]<-NA
        } else {
          restat$values<-suppressWarnings(as.numeric(levels(restat$values))[restat$values])        
        }
      } 
    }
    if (cr&cache&(!is.null(restat))){
      force(oname<-paste0("b_",id,"-",udate,"-",sum(keep_flags),"-",sum(cflags),sub("-$","",paste0("-",select_freq),perl=TRUE)))
      pl<-put_eurostat_cache(restat,oname,update_cache,cache_dir,compress_file)
      if (verbose){message("The data was cached ",pl,".\n")}
    }
    if (label & !is.null(restat)){
      if (verbose) {message("restat - nrow:",nrow(restat),";ncol:",ncol(restat))}
      dsd<-get_eurostat_dsd(id,verbose=verbose)
      if (!is.null(dsd)){
        if (verbose) {message("dsd - nrow:",nrow(dsd),";ncol:",ncol(dsd))}
        cn<-colnames(restat)[!(colnames(restat) %in% c("time","values","flags"))]
        restat<-data.table::data.table(restat,stringsAsFactors=stringsAsFactors) 
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
# check_tf<-function(x,tf){
#   any(sapply(tf,grepl,x=x,perl=TRUE))
# }



