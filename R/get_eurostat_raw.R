#' @title Get Eurostat data as it is 
#' @description Download data sets from \href{https://ec.europa.eu/eurostat}{Eurostat} database .
#' @param id A code name for the dataset of interest.
#'        See \code{\link{search_eurostat_toc}} for details how to get an id.
#' @param mode defines the format of the downloaded dataset. It can be \code{txt} (the default value) for 
#'        Tab Separated Values (TSV), or \code{csv} for SDMX-CSV, or \code{xml} for the SDMX-ML version. 
#' @param cache a logical whether to do caching. Default is \code{TRUE}.
#' @param update_cache a logical with a default value \code{FALSE}, whether to update cache. Can be set also with
#'        \code{options(restatapi_update=TRUE)}
#' @param cache_dir a path to a cache directory. The \code{NULL} (default) uses the memory as cache. 
#'        If the folder  if the \code{cache_dir} directory does not exist it saves in the 'restatapi' directory 
#'        under the temporary directory from \code{tempdir()}. Directory can also be set with
#'        \code{option(restatapi_cache_dir=...)}.
#' @param compress_file a logical whether to compress the
#'        RDS-file in caching. Default is \code{TRUE}.
#' @param stringsAsFactors if \code{TRUE} the variables which are not numeric are
#'        converted to factors. The default value \code{FALSE}, in this case they are returned as characters.
#' @param keep_flags a logical whether the observation status (flags) - e.g. "confidential",
#'        "provisional", etc. - should be kept in a separate column or if they
#'        can be removed. Default is \code{FALSE}. For flag values see: 
#'        \url{https://ec.europa.eu/eurostat/data/database/information}.
#' @param check_toc a boolean whether to check the provided \code{id} in the Table of Contents (TOC) or not. The default value 
#'        \code{FALSE}, in this case the base URL for the download link is retrieved from the configuration file. 
#'        If the value is \code{TRUE} then the TOC is downloaded and the \code{id} is checked in it. If it found then the download link 
#'        is retrieved form the TOC.  
#' @param melt a boolean with default value \code{TRUE} and used only if the \code{mode="txt"}. In case it is \code{FALSE}, 
#'        the downloaded tsv file is not melted, the time dimension remains in columns and it does not process the flags.         
#' @param verbose A boolean with default \code{FALSE}, so detailed messages (for debugging) will not printed.
#'         Can be set also with \code{options(restatapi_verbose=TRUE)}        
#' @param ... further argument for the \code{\link{load_cfg}} function
#' @export
#' 
#' @details Data sets are downloaded from \href{https://wikis.ec.europa.eu/display/EUROSTATHELP/Transition+-+from+Eurostat+Bulk+Download+to+API}{the Eurostat bulk download facility} 
#' in CSV, TSV or SDMX format.
#' 
#' 
#' The \code{id}, should be a value from the \code{code} column of the table of contents (\code{\link{get_eurostat_toc}}), and can be searched for with the \code{\link{search_eurostat_toc}} function. The id value can be retrieved from the \href{https://ec.europa.eu/eurostat/data/database}{Eurostat database}
#'  as well. The Eurostat database gives codes in the Data Navigation Tree after every dataset in parenthesis.
#' By default all datasets downloaded in TSV format and cached as they are often rather large. 
#' The datasets cached in memory (default) or can be stored in a temporary directory if \code{cache_dir} or \code{option(restatpi_cache_dir)} is defined.
#' The cache can be emptied with \code{\link{clean_restatapi_cache}}.
#' If the \code{id} is checked in TOC then the data will saved in the cache with the date from the "lastUpdate" column from the TOC, otherwise it is saved with the current date.  
#' @return a data.table with the following columns if the default \code{melt=TRUE} is used:
#'  \tabular{ll}{
#'  \code{FREQ} \tab The frequency of the data (\strong{A}nnual, \strong{S}emi-annual, \strong{H}alf-year, \strong{Q}uarterly, \strong{M}onthly,
#'  \strong{W}eekly, \strong{D}aily)\cr
#'  dimension names \tab One column for each dimension in the data\cr
#'  \code{TIME_FORMAT} \tab A column for the time format, if the source file SDMX-ML and the data was not
#'  loaded from a previously cached TSV download (this column is missing if the source file is TSV)\cr
#'  \code{time/TIME_PERIOD} \tab A column for the time dimension, where the name of the column depends on the
#'  source file (TSV/SDMX-ML)\cr
#'  \code{values/OBS_VALUE} \tab A column for numerical values, where the name of the column depends on the
#'  source file (TSV/SDMX-ML)\cr
#'  \code{flags/OBS_STATUS} \tab A column for flags if the \code{keep_flags=TRUE} otherwise this column is not included
#'  in the data table, and the name of the column depends on the source file (TSV/SDMX-ML)
#'  }
#' The data does not include all missing values. The missing values are dropped if the value and flags are missing
#' on a particular time. 
#' 
#' In case \code{melt=FALSE} the results is a data.table where the first column contains the comma separated values of the various dimensions, and the columns contains the observations for each time dimension.   
#' 
#' @seealso \code{\link{get_eurostat_data}}, \code{\link{get_eurostat_bulk}}
#' @examples 
#' \dontshow{
#' if (parallel::detectCores()<=2){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }    
#' }
#' \donttest{
#' options(timeout=2)
#' dt<-get_eurostat_raw("agr_r_milkpr",keep_flags=TRUE)
#' dt<-get_eurostat_raw("avia_par_ee",mode="xml",check_toc=TRUE,update_cache=TRUE)
#' options(restatapi_update=FALSE)
#' dt<-get_eurostat_raw("avia_par_me",mode="txt",cache_dir=tempdir(),compress_file=FALSE,verbose=TRUE)
#' options(timeout=60)
#' }

get_eurostat_raw <- function(id, 
                             mode="txt",
                             cache=TRUE, 
                             update_cache=FALSE,
                             cache_dir=NULL,
                             compress_file=TRUE,
                             stringsAsFactors=FALSE,
                             keep_flags=FALSE,
                             check_toc=FALSE,
                             melt=TRUE,
                             verbose=FALSE,...){
  
  restat_raw<-raw<-NULL
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  update_cache<-update_cache|getOption("restatapi_update", FALSE)
  dmethod<-getOption("restatapi_dmethod",get("dmethod",envir=restatapi::.restatapi_env))
  tbc<-TRUE #to be continued to the next steps 
  if (verbose)  {message("\nget_eurostat_raw - API version:",get("rav",envir=restatapi::.restatapi_env))}
  if((!exists(".restatapi_env")|(length(list(...))>0))){
    if ((length(list(...))>0)) {
      if (all(names(list(...)) %in% c("api_version","load_toc","parallel","max_cores","verbose"))){
        load_cfg(...)  
      } else {
        load_cfg()
      }
    } else {
      load_cfg()
    }  
  }
  cfg<-get("cfg",envir=restatapi::.restatapi_env) 
  rav<-get("rav",envir=restatapi::.restatapi_env)
  # if (verbose)  {message("get_eurostat_raw - API version:",rav)}
  if (!is.null(id)){id<-tolower(trimws(id))} else {
    tbc<-FALSE
    message("The dataset 'id' is missing.")
  }
  if (!melt) {
    cache=FALSE  # not cache if not melted
    if (mode!="txt"){
      message("It is not possible to have raw casted table with mode='csv' or mode='xml'.")
    }
  }
  
  if (tbc){
    if (check_toc){
      toc<-restatapi::get_eurostat_toc(verbose=verbose)
      if (is.null(toc)){
        message("The TOC is missing. Could not get the download link.")
        tbc<-FALSE
      } else {
        if (any(grepl(id,toc$code,ignore.case=TRUE))){
          udate<-toc$lastUpdate[grepl(id,toc$code,ignore.case=TRUE)]
          if (mode=="txt") {
            bulk_url<-toc$downloadLink.tsv[grepl(id,toc$code,ignore.case=TRUE)]
          } else if (mode=="xml") {
            bulk_url<-toc$downloadLink.sdmx[grepl(id,toc$code,ignore.case=TRUE)]
          } else {
            message("Incorrect mode:",mode,"\n It should be either 'txt' or 'xml'." )
            tbc<-FALSE
          }
          if (length(bulk_url)==0|is.na(bulk_url)){
            message("There is no downloadlink in the TOC for ",id)
            tbc<-FALSE
          }
          if (verbose) {message("get_eurostat_raw - raws of TOC: ",nrow(toc),
                                "\nget_eurostat_raw - bulk url: ",bulk_url,
                                "\nget_eurostat_raw - data rowcount in TOC: ",toc$values[grepl(id,toc$code,ignore.case=TRUE)])}
        } else {
          message(paste0("'",id,"' is not in the table of contents. Please check if the 'id' is correctly spelled."))
          tbc<-FALSE
        }
      }
    }else{
      udate<-format(Sys.Date(),"%Y.%m.%d")
      if (mode=="csv") {
        bulk_url_base<-eval(parse(text=paste0("cfg$BULK_BASE_URL$'",rav,"'$ESTAT")))
        if (keep_flags) {
          bulk_url_end<- paste0(id,"?format=SDMX-CSV&compressed=true")
        } else {
          bulk_url_end<- paste0(id,"?format=SDMX-CSV&detail=dataonly&compressed=true")
        }
        bulk_url<-paste0(bulk_url_base,bulk_url_end)
        if (verbose) {message("get_eurostat_raw - bulk url: ",bulk_url)}
      }else if (mode=="txt") {
        bulk_url_base<-eval(parse(text=paste0("cfg$BULK_BASE_URL$'",rav,"'$ESTAT")))
        bulk_url_end<- switch(rav,"1" = paste0("?file=data/",id,".tsv.gz"),"2"= paste0(id,"?format=TSV&compressed=true"))
        bulk_url<-paste0(bulk_url_base,bulk_url_end)
        if (verbose) {message("get_eurostat_raw - bulk url: ",bulk_url)}
      } else if (mode=="xml") {
        bulk_url_base<-eval(parse(text=paste0("cfg$BULK_BASE_URL$'",rav,"'$ESTAT")))
        bulk_url_end<- switch(rav,"1" = paste0("?file=data/",id,".sdmx.zip"),"2"= paste0(id,"?format=sdmx_2.1_structured&compressed=true"))
        bulk_url<-paste0(bulk_url_base,bulk_url_end)
        if (verbose) {message("get_eurostat_raw - bulk url: ",bulk_url)}
      } else {
        message("Incorrect mode:",mode,"\n It should be either 'csv', 'txt' (default) or 'xml'." )
        tbc<-FALSE
      }
    }
  }
  
  
  
  if (tbc){
    if ((cache)&(!update_cache)) {
      restat_raw<-data.table::copy(restatapi::get_eurostat_cache(paste0("r_",id,"-",udate,"-",sum(keep_flags)),cache_dir,verbose=verbose))
    }
    if ((!cache)|(is.null(restat_raw))|(update_cache)){
      if (mode=="csv"){
        if(max(utils::sessionInfo()$otherPkgs$data.table$Version,utils::sessionInfo()$loadedOnly$data.table$Version)>"1.11.7"){
          tryCatch({raw<-data.table::fread(text=readLines(gzcon(url(bulk_url))),sep=',',sep2=',',colClasses='character',header=TRUE,stringsAsFactors=stringsAsFactors)},
                   error = function(e) {
                     if (verbose){message("get_eurostat_raw - Error by the reading in with data.table the downloaded CSV file:",'\n',paste(unlist(e),collapse="\n"))}
                     else {message("There is an error by the download of the CSV file. Run the same command with verbose=TRUE option to get more info on the issue.")}
                     tbc<-FALSE
                   },
                   warning = function(w) {
                     if (verbose){message("get_eurostat_raw - Warning by the reading in with data.table the downloaded CSV file:",'\n',paste(unlist(w),collapse="\n"))}
                     else {message("There is a warning by the download of the CSV file. Run the same command with verbose=TRUE option to get more info on the issue.")}
                   })
        } else{
          tryCatch({raw<-data.table::fread(paste(readLines(gzcon(url(bulk_url))),collapse="\n"),sep=',',sep2=',',colClasses='character',header=TRUE,stringsAsFactors=stringsAsFactors)},
                   error = function(e) {
                     if (verbose){message("get_eurostat_raw - Error by the reading in with data.table the downloaded CSV file:",'\n',paste(unlist(e),collapse="\n"))}
                     else {message("There is an error by the download of the CSV file. Run the same command with verbose=TRUE option to get more info on the issue.")}
                     tbc<-FALSE
                   },
                   warning = function(w) {
                     if (verbose){message("get_eurostat_raw - Warning by the reading in with data.table the downloaded CSV file:",'\n',paste(unlist(w),collapse="\n"))}
                     else {message("There is a warning by the download of the CSV file. Run the same command with verbose=TRUE option to get more info on the issue.")}
                   })
        }
        if(!is.null(raw)){
          if(ncol(raw)==1){
            data.table::setnames(raw,"v1")
            raw<-as.character(raw$v1)
            if (any(grepl(paste0(id, ".* does not exist"),raw))){
              message("The file ",gsub(".*/","",bulk_url)," does not exist or is not readable on the server. Try to download with the check_toc=TRUE option.")
              tbc<-FALSE
            } 
          }
        }  
        if (tbc){
          restat_raw<-data.table::copy(raw)
          restat_raw[, c("DATAFLOW", "LAST UPDATE") := NULL]
          rm(raw)
          # restat_raw$OBS_VALUE<-gsub('^\\:$',"",restat_raw$OBS_VALUE,perl=TRUE)
          # restat_raw$OBS_VALUE<-gsub('[^0-9\\.\\-\\:]',"",restat_raw$OBS_VALUE,perl=TRUE)
          restat_raw<-data.table::data.table(restat_raw,stringsAsFactors=stringsAsFactors)
        }
      } else if (mode=="txt"){
            if(max(utils::sessionInfo()$otherPkgs$data.table$Version,utils::sessionInfo()$loadedOnly$data.table$Version)>"1.11.7"){
              tryCatch({raw<-data.table::fread(text=readLines(gzcon(url(bulk_url))),sep='\t',sep2=',',colClasses='character',header=TRUE,stringsAsFactors=stringsAsFactors)},
                       error = function(e) {
                         if (verbose){message("get_eurostat_raw - Error by the reading in with data.table the downloaded TSV file:",'\n',paste(unlist(e),collapse="\n"))}
                         else {message("There is an error by the download of the TSV file. Run the same command with verbose=TRUE option to get more info on the issue.")}
                         tbc<-FALSE
                       },
                       warning = function(w) {
                         if (verbose){message("get_eurostat_raw - Warning by the reading in with data.table the downloaded TSV file:",'\n',paste(unlist(w),collapse="\n"))}
                         else {message("There is a warning by the download of the TSV file. Run the same command with verbose=TRUE option to get more info on the issue.")}
                       })
            } else{
              tryCatch({raw<-data.table::fread(paste(readLines(gzcon(url(bulk_url))),collapse="\n"),sep='\t',sep2=',',colClasses='character',header=TRUE,stringsAsFactors=stringsAsFactors)},
                       error = function(e) {
                         if (verbose){message("get_eurostat_raw - Error by the reading in with data.table the downloaded TSV file:",'\n',paste(unlist(e),collapse="\n"))}
                         else {message("There is an error by the download of the TSV file. Run the same command with verbose=TRUE option to get more info on the issue.")}
                         tbc<-FALSE
                       },
                       warning = function(w) {
                         if (verbose){message("get_eurostat_raw - Warning by the reading in with data.table the downloaded TSV file:",'\n',paste(unlist(w),collapse="\n"))}
                         else {message("There is a warning by the download of the TSV file. Run the same command with verbose=TRUE option to get more info on the issue.")}
                       })
            }
            if(!is.null(raw)){
              if(ncol(raw)==1){
                data.table::setnames(raw,"v1")
                raw<-as.character(raw$v1)
                if (any(grepl(paste0(id, ".* does not exist"),raw))){
                  message("The file ",gsub(".*/","",bulk_url)," does not exist or is not readable on the server. Try to download with the check_toc=TRUE option.")
                  tbc<-FALSE
                }
              }  
            }

            if (tbc) {
              if(melt) {
                cname<-colnames(raw)[1] 
                if (is.character(cname)){
                  cnames<-utils::head(unlist(strsplit(cname,(',|\\\\'))),-1)
                  rname<-switch(rav, "1" = utils::tail(unlist(strsplit(cname,(',|\\\\'))),1),"2"="time")
                  if (verbose) {message("get_eurostat_raw - class(raw): ",class(raw))}
                  data.table::setnames(raw,1,"bdown")
                  raw_melted<-data.table::melt.data.table(raw,"bdown",variable.factor=stringsAsFactors)
                  rm(raw)
                  data.table::setnames(raw_melted,2:3,c(rname,"values"))
                  raw_melted<-raw_melted[raw_melted$values!=":",]
                  if (check_toc|rav==1){
                    FREQ<-gsub("MD","D",gsub('[0-9\\.\\-]',"",raw_melted$time))
                    FREQ[FREQ==""]<-"A"
                  }
                  restat_raw<-data.table::as.data.table(data.table::tstrsplit(raw_melted$bdown,",",fixed=TRUE),stringsAsFactors=stringsAsFactors)
                  data.table::setnames(restat_raw,cnames)  
                  restat_raw<-data.table::data.table(restat_raw,raw_melted[,2:3],stringsAsFactors=stringsAsFactors)
                  if (check_toc|rav==1) {restat_raw<-data.table::data.table(FREQ,restat_raw)}
                  if (keep_flags) {restat_raw$flags<-gsub('[0-9\\.\\-\\s\\:]',"",restat_raw$values,perl=TRUE)}
                  restat_raw$values<-gsub('^\\:$',"",restat_raw$values,perl=TRUE)
                  restat_raw$values<-gsub('[^0-9\\.\\-\\:]',"",restat_raw$values,perl=TRUE)
                  restat_raw<-data.table::data.table(restat_raw,stringsAsFactors=stringsAsFactors)  
                } else {
                  message("The file download was not successful. Try again later.")
                }   
              } else {
                restat_raw<-raw
                cache<-update_cache<-FALSE
              }
            }  
          # }
        # }
      } else if (mode=="xml"){
        format<-switch(rav, "1" = "zip", "2" = "gz")
        if (check_toc) {format<-"zip"}
        if (verbose) {message("get_eurostat_raw - file format: ",format)}
        sdmx_file<-restatapi::get_compressed_sdmx(bulk_url,verbose=verbose,format=format)
        if(!is.null(sdmx_file)){
          xml_mark<-switch(rav, "1" = ".//data:Series", "2" = ".//Series")
          if (check_toc) {xml_mark<-".//data:Series"}
          xml_leafs<-xml2::xml_find_all(sdmx_file,xml_mark)
          if (verbose) {message("get_eurostat_raw - class(xml_leafs): ",class(xml_leafs),
                                "\nget_eurostat_raw - number of nodes: ",length(xml_leafs),
                                "\nget_eurostat_raw - number of cores: ",getOption("restatapi_cores",1L))}
          if (Sys.info()[['sysname']]=='Windows'){
            if (getOption("restatapi_cores",1L)==1) {
              if (verbose) message("No parallel")
              restat_raw<-data.table::rbindlist(lapply(xml_leafs,extract_data,keep_flags=keep_flags,stringsAsFactors=stringsAsFactors,check_toc=check_toc))
            } else {
              xml_leafs<-as.character(xml_leafs)
              cl<-parallel::makeCluster(getOption("restatapi_cores",1L))
              parallel::clusterEvalQ(cl,require(xml2))
              parallel::clusterExport(cl,c("extract_data"))
              restat_raw<-data.table::rbindlist(parallel::parLapply(cl,xml_leafs,extract_data,keep_flags=keep_flags,stringsAsFactors=stringsAsFactors,check_toc=check_toc))              
              parallel::stopCluster(cl)  
            }
          }else{
            restat_raw<-data.table::rbindlist(parallel::mclapply(xml_leafs,extract_data,keep_flags=keep_flags,stringsAsFactors=stringsAsFactors,check_toc=check_toc,mc.cores=getOption("restatapi_cores",1L)))                                  
          }
        } else{
          message("Could not download the SDMX file, use the verbose option to see the exact cause of the error.")
        }
      }
    }
    if (!is.null(restat_raw)&melt){
      restat_raw[]
      if (any(sapply(restat_raw,is.factor))&(!stringsAsFactors)) {
        col_conv<-colnames(restat_raw)
        restat_raw[,col_conv]<-restat_raw[,lapply(.SD,as.character),.SDcols=col_conv][]
      }
      if (!(any(sapply(restat_raw,is.factor)))&(stringsAsFactors)) {
        restat_raw<-data.table::data.table(restat_raw,stringsAsFactors=stringsAsFactors)
      }
      if ((!keep_flags) & ("OBS_STATUS" %in% colnames(restat_raw)))  {restat_raw$OBS_STATUS<-NULL}
    }
    if (verbose) {message("get_eurostat_raw - caching in raw: ",all(!grepl("get_eurostat_bulk|get_eurostat_data",as.character(sys.calls()),perl=TRUE)),
                          "\nget_eurostat_raw - local filter: ",exists("local_filter",envir=sys.parent(1)),
                          "\nget_eurostat_raw - called from: ",as.character(sys.call()),
                          "\nget_eurostat_raw - get_eurostat_raw in sys.call(): ", grepl("^get_eurostat_rawidtxt",paste0(as.character(sys.call()),collapse="")))}
    #check if the function was called from the get_eurostat_data function
    if (grepl("^get_eurostat_rawidtxt",paste0(as.character(sys.call()),collapse=""))&any(grepl("get_eurostat_data",as.character(sys.calls())))){  
      #if yes get the value of local_filter and force_local_filter from the call
      if (exists("local_filter",envir=sys.parent(1))) {plf<-get("local_filter",envir=sys.parent(1))} else {plf<-FALSE}
      if (exists("force_local_filter",envir=sys.parent(1))) {pflf<-get("force_local_filter",envir=sys.parent(1))} else {pflf<-FALSE}
      child_cache<-plf|pflf
      if (verbose) {message("get_eurostat_raw - ",plf,pflf,child_cache)}
    } else {
      child_cache<-FALSE
    }
        
    if ((!is.null(restat_raw))&cache&(all(!grepl("get_eurostat_bulk|get_eurostat_data",as.character(sys.calls()),perl=TRUE))|child_cache)){
      oname<-paste0("r_",id,"-",udate,"-",sum(keep_flags))
      pl<-restatapi::put_eurostat_cache(restat_raw,oname,update_cache,cache_dir,compress_file)
      if ((!is.null(pl))&(verbose)) {message("get_eurostat_raw - The raw data was cached ",pl,"." )}
    }
  }
  return(restat_raw)
}
