#' @title Download the Data Structure Definition of a dataset
#' @description Download Data Structure Definition (DSD) of a Eurostat dataset if it is not cached previously. 
#' @param id a character string with the id of the dataset. It is a value from the \code{codename} column of the \code{\link{get_eurostat_toc}} function. 
#' @param lang a character string either \code{en}, \code{de} or \code{fr} to define the language version for the name column of the DSD. It is used only in the new API. The default is \code{en} - English.
#' @param cache a boolean whether to load/save the DSD from/in the cache or not. The default value is \code{TRUE}, so that the DSD is checked first in the cache and if does not exist then downloaded from Eurostat and cached.
#' @param update_cache a boolean to update cache or not. The default value is \code{FALSE}, so the cache is not updated. Can be set also with \code{options(restatapi_update=TRUE)}
#' @param cache_dir a path to a cache directory. The default is \code{NULL}, in this case the DSD is cached in the memory (in the '.restatapi_env'). Otherwise if the \code{cache_dir} directory does not exist it creates the 'restatapi' directory in the temporary directory from \code{tempdir()} to save the RDS-file. Directory can also be set with \code{option(restatapi_cache_dir=...)}.
#' @param compress_file a logical whether to compress the RDS-file in caching. Default is \code{TRUE}.
#' @param verbose A boolean with default \code{FALSE}, so detailed messages (for debugging) will not printed.
#'         Can be set also with \code{options(restatapi_verbose=TRUE)}
#' @param ... parameter to pass on the \code{load_cfg} function
#' @return If the DSD does not exist it returns \code{NULL} otherwise the result is a table with the 3 columns:
#'    \tabular{ll}{
#'      \code{concept} \tab The name of the concepts in the order of the data structure \cr
#'      \code{code} \tab The possible list of codes under the concept \cr
#'      \code{name} \tab The name/description of the code 
#'    }
#' @export
#' @seealso \code{\link{get_eurostat_data}}, \code{\link{search_eurostat_toc}}.
#' @details The DSD is downloaded from Eurostat's website, through the REST API in XML (SDMX-ML) format.
#'  
#' @references For more information see the detailed documentation of the \href{https://ec.europa.eu/eurostat/data/web-services}{API}. 
#' @examples 
#' \dontshow{
#' if (parallel::detectCores()<=2){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }
#' }
#' \donttest{
#' if (!(grepl("amzn|-aws|-azure ",Sys.info()['release']))) options(timeout=2)
#' head(get_eurostat_dsd("med_rd6",lang="de",cache=FALSE,verbose=TRUE))
#' options(timeout=60)
#' }

get_eurostat_dsd <- function(id,
                             lang="en",
                             cache=TRUE,
                             update_cache=FALSE,
                             cache_dir=NULL,
                             compress_file=TRUE,
                             verbose=FALSE,...) {
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  dmethod<-getOption("restatapi_dmethod",get("dmethod",envir=restatapi::.restatapi_env))
  if (getOption("restatapi_cores",1L)>=parallel::detectCores()) options(restatapi_cores=parallel::detectCores()-1)
  # if (verbose)  {message("\nget_eurostat_dsd - API version:",get("rav",envir=restatapi::.restatapi_env))}
  tbc<-TRUE #to be continued for the next steps
  if (is.null(id)){
    warning('No dataset id were provided.')
    dsd<-NULL
  } else {
    dsd<-dsd_xml<-cc_xml<-NULL
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
   
    
    update_cache <- update_cache | getOption("restatapi_update", FALSE)
    if ((cache) & (!update_cache)) {
      dsd<-restatapi::get_eurostat_cache(paste0(id,".",lang,".dsd"),cache_dir,verbose=verbose)
    }
    if ((!cache)|(is.null(dsd))|(update_cache)){
      cfg<-get("cfg",envir=restatapi::.restatapi_env) 
      rav<-get("rav",envir=restatapi::.restatapi_env)
      if (verbose)  {message("\nget_eurostat_dsd - API version:",rav)}
      dsd_endpoint <- paste0(eval(parse(text=paste0("cfg$QUERY_BASE_URL$'",rav,"'$ESTAT$metadata$'2.1'$datastructure"))),"/", 
                             eval(parse(text=paste0("cfg$QUERY_PRIOR_ID$'",rav,"'$ESTAT$metadata"))),id,"?",
                             eval(parse(text=paste0("cfg$QUERY_PARAMETERS$'",rav,"'$metadata[2]"))),"=",
                             eval(parse(text=paste0("cfg$DATAFLOW_REFERENCES$'",rav,"'$datastructure[1]")))
                             )
      temp<-tempfile()
      if (verbose) {
        message("\nget_eurostat_dsd - Trying to download the DSD from: ",dsd_endpoint)
        tryCatch({utils::download.file(dsd_endpoint,temp,dmethod)},
                 error = function(e) {
                   message("get_eurostat_dsd - Error by the download of the DSD file:",'\n',paste(unlist(e),collapse="\n"))
                   dsd_xml<-NULL
                 },
                 warning = function(w) {
                   message("get_eurostat_dsd - Warning by the download of the DSD file:",'\n',paste(unlist(w),collapse="\n"))
                   tbc<-FALSE
                   dsd_xml<-NULL
                 })
        message("\nget_eurostat_dsd - class(temp): ",class(temp)," - is.na(temp): ",is.na(temp)," - file.size(temp): ", file.size(temp)," - temp: ",temp)
        if (is.na(file.size(temp))) {temp_size=0} else {temp_size=file.size(temp)}
        if (temp_size!=0 & tbc) {
          message("Trying to extract the DSD from: ",temp)
          tryCatch({dsd_xml<-xml2::read_xml(temp)},
                 error = function(e) {
                   message("get_eurostat_dsd - Error during the extraction of the XML from the downloaded DSD file:",'\n',paste(unlist(e),collapse="\n"))
                   dsd_xml<-NULL
                 },
                 warning = function(w) {
                   message("get_eurostat_dsd - There is warning by the extraction of the XML from the downloaded DSD file:",'\n',paste(unlist(w),collapse="\n"))
                   dsd_xml<-NULL
                 })
        } else {
          dsd_xml<-NULL
        }
      } else {
        tryCatch({utils::download.file(dsd_endpoint,temp,dmethod,quiet=TRUE)},
                 error = function(e) {message("get_eurostat_dsd - There is an error by the download of the DSD file. Run the same command with verbose=TRUE option to get more info on the issue.")
                   dsd_xml<-NULL
                 },
                 warning = function(w) {message("get_eurostat_dsd - There is a warning by the download of the DSD file. Run the same command with verbose=TRUE option to get more info on the issue.")
                   tbc<-FALSE
                   dsd_xml<-NULL
                 })
        if (is.na(file.size(temp))) {temp_size=0} else {temp_size=file.size(temp)}
        if (temp_size!=0 & tbc) {
          tryCatch({dsd_xml<-xml2::read_xml(temp)},
                 error = function(e) {message("get_eurostat_dsd - There is an error by the reading of the downloaded DSD file. Run the same command with verbose=TRUE option to get more info on the issue.")
                   dsd_xml<-NULL
                 },
                 warning = function(w) {message("get_eurostat_dsd - There is an error by the reading of the downloaded DSD file. Run the same command with verbose=TRUE option to get more info on the issue.")
                   dsd_xml<-NULL
                 })
        } else {
          dsd_xml<-NULL
        }
      }
      unlink(temp)
      if (!is.null(dsd_xml)){
        prefix<-switch(rav,"1"="str","2"="s")
        concepts<-xml2::xml_attr(xml2::xml_find_all(dsd_xml,paste0("//",prefix,":ConceptIdentity//Ref")),"id")
        concepts<-concepts[!(toupper(concepts) %in% c("TIME_PERIOD","OBS_VALUE"))]
        if (verbose)  {message("get_eurostat_dsd - concepts:",paste(concepts,collapse = ","))}
        if (Sys.info()[['sysname']]=='Windows'){
          if (verbose) {message(class(concepts),"\nnumber of nodes: ",length(concepts),"\nnumber of cores: ",getOption("restatapi_cores",1L),"\n")}
          if (getOption("restatapi_cores",1L)==1) {
            if (verbose) message("No parallel")
            dsd<-data.table::data.table(do.call(rbind,lapply(concepts,restatapi::extract_dsd,dsd_xml=dsd_xml,lang=lang)),stringsAsFactors=FALSE)
            # if (verbose)  {message("get_eurostat_dsd - dsd type 1:",class(dsd))}
          } else {
            dsd_xml<-as.character(dsd_xml)
            cl<-parallel::makeCluster(getOption("restatapi_cores",1L))
            parallel::clusterEvalQ(cl,require(xml2))
            parallel::clusterEvalQ(cl,require(restatapi))
            # parallel::clusterExport(cl,c("extract_dsd"))
            parallel::clusterExport(cl,c("dsd_xml"),envir=environment())
            dsd<-data.table::data.table(do.call(rbind,parallel::parLapply(cl,concepts,restatapi::extract_dsd,dsd_xml=dsd_xml,lang=lang)),stringsAsFactors=FALSE)
            parallel::stopCluster(cl)
          }
        }else{
          dsd<-data.table::data.table(do.call(rbind,parallel::mclapply(concepts,restatapi::extract_dsd,dsd_xml=dsd_xml,lang=lang,mc.cores=getOption("restatapi_cores",1L))),stringsAsFactors=FALSE)
          # if (verbose)  {message("get_eurostat_dsd - dsd type 2:",class(dsd))}
        }  
        if (verbose) {message("get_eurostat_dsd - DSD NULL:",is.null(dsd))}
        if (!is.null(dsd)) {names(dsd)<-c("concept","code","name")}

  #get content constraint (cc)
        
        if (!is.null(dsd)){
          cc_endpoint <- paste0(eval(parse(text=paste0("cfg$QUERY_BASE_URL$'",rav,"'$ESTAT$metadata$'2.1'$contentconstraint"))),"/", 
                                eval(parse(text=paste0("cfg$QUERY_PRIOR_ID$'",rav,"'$ESTAT$metadata"))),id)
          temp<-tempfile()
          if (verbose) {
            message("get_eurostat_dsd - Trying to download the CC from: ",cc_endpoint)
            tryCatch({utils::download.file(cc_endpoint,temp,dmethod)},
                     error = function(e) {
                       message("get_eurostat_dsd - Error by the download of the CC file:",'\n',paste(unlist(e),collapse="\n"))
                       cc_xml<-NULL
                     },
                     warning = function(w) {
                       message("get_eurostat_dsd - Warning by the download of the CC file:",'\n',paste(unlist(w),collapse="\n"))
                       tbc<-FALSE
                       cc_xml<-NULL
                     })
            if (is.na(file.size(temp))) {temp_size=0} else {temp_size=file.size(temp)}
            if (temp_size!=0 & tbc) {
              message("get_eurostat_dsd - Trying to extract the CC from: ",temp)
              tryCatch({cc_xml<-xml2::read_xml(temp)},
                       error = function(e) {
                         message("get_eurostat_dsd - Error during the extraction of the XML from the downloaded CC file:",'\n',paste(unlist(e),collapse="\n"))
                         cc_xml<-NULL
                       },
                       warning = function(w) {
                         message("get_eurostat_dsd - There is warning by the extraction of the XML from the downloaded CC file:",'\n',paste(unlist(w),collapse="\n"))
                         cc_xml<-NULL
                       })
            } else {
              cc_xml<-NULL
            }
          } else {
            tryCatch({utils::download.file(cc_endpoint,temp,dmethod,quiet=TRUE)},
                     error = function(e) {message("There is an error by the download of the content constraint file. Run the same command with verbose=TRUE option to get more info on the issue.")
                       tbc<-FALSE
                       cc_xml<-NULL
                     },
                     warning = function(w) {message("There is an error by the download of the content constraint file. Run the same command with verbose=TRUE option to get more info on the issue.")
                       tbc<-FALSE
                       cc_xml<-NULL
                     })
            if (is.na(file.size(temp))) {temp_size=0} else {temp_size=file.size(temp)}
            if (temp_size!=0 & tbc) {
              tryCatch({cc_xml<-xml2::read_xml(temp)},
                       error = function(e) {message("There is an error by the reading of the downloaded CC file. Run the same command with verbose=TRUE option to get more info on the issue.")
                         cc_xml<-NULL
                       },
                       warning = function(w) {message("There is a warning by the reading of the downloaded CC file. Run the same command with verbose=TRUE option to get more info on the issue.")
                         cc_xml<-NULL
                       })
            } else {
              cc_xml<-NULL
            }
          }
          unlink(temp)
          if (!is.null(cc_xml)){
            cconcepts<-xml2::xml_attr(xml2::xml_find_all(cc_xml,"//c:KeyValue"),"id")
            if (verbose) {message(class(cconcepts),"\nnumber of nodes: ",length(cconcepts),"\nnumber of cores: ",getOption("restatapi_cores",1L),"\n")}
            ft_dsd<-data.table::data.table(do.call(rbind,lapply(cconcepts,filter_dsd,cc_xml=cc_xml, dsd=dsd)),stringsAsFactors=FALSE)
            dsd<-ft_dsd
            # if (verbose)  {message("get_eurostat_dsd - dsd type 3:",class(dsd))}
            
          } else {
            dsd<-NULL
          }
        }
        
        if (cache){
          pl<-restatapi::put_eurostat_cache(dsd,paste0(id,".",lang,".dsd"),update_cache,cache_dir,compress_file)
          if (verbose) {message("get_eurostat_dsd - The DSD of the ",id," dataset was cached ",pl,".")}
        }  
      } else {
#       dsd<-NULL
        if (verbose) {
          message("get_eurostat_dsd - The dsd_xml is NULL. Please check in a browser the url below. If it provides valid response you can try again to download the DSD.\n ",dsd_endpoint)
        }
      }
    }
    if (!is.null(dsd)){
      # if (verbose)  {message("get_eurostat_dsd - dsd type 4:",class(dsd))}
      data.table::as.data.table(dsd,stringsAsFactors=FALSE)
      # if (verbose)  {message("get_eurostat_dsd - dsd type 5:",class(dsd))}
    }
  }
  return(dsd)
}

filter_dsd<-function(x,cc_xml,dsd){
  cc_values<-xml2::xml_text(xml2::xml_children(xml2::xml_find_all(cc_xml,paste0('//c:KeyValue[@id="',x,'"]'))))
  return(dsd[(dsd$concept==x) & (dsd$code %in% cc_values),])
}

