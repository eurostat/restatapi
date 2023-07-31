#' @title Download the codelist of a concept
#' @description Download codelist of a concept from Eurostat if it is not cached previously. 
#' @param id a character string with id of the concept. It is a value from the \code{concept} column of the \code{\link{get_eurostat_dsd}} function. 
#' @param lang a character string either \code{en}, \code{de} or \code{fr} to define the language version for the name column of the codelist. It is used only in the new API. The default is \code{en} - English.
#' @param cache a boolean whether to load/save the TOC from/in the cache or not. The default value is \code{TRUE}, so that the TOC is checked first in the cache and if does not exist then downloaded from Eurostat and cached.
#' @param update_cache a boolean to update cache or not. The default value is \code{FALSE}, so the cache is not updated. Can be set also with \code{options(restatapi_update=TRUE)}
#' @param cache_dir a path to a cache directory. The default is \code{NULL}, in this case the TOC is cached in the memory (in the '.restatapi_env'). Otherwise if the \code{cache_dir} directory does not exist it creates the 'restatapi' directory in the temporary directory from \code{tempdir()} to save the RDS-file. Directory can also be set with \code{option(restatapi_cache_dir=...)}.
#' @param compress_file a logical whether to compress the RDS-file in caching. Default is \code{TRUE}.
#' @param verbose A boolean with default \code{FALSE}, so detailed messages (for debugging) will not printed.
#'         Can be set also with \code{options(restatapi_verbose=TRUE)}
#' @param ... parameter to pass on the \code{load_cfg} function
#' @return If the codelist does not exist it returns \code{NULL} otherwise the result is a table with the 2 columns:
#'    \tabular{ll}{
#'      \code{code} \tab All the possible codes under the concept \cr
#'      \code{name} \tab The name/description of the code 
#'    }
#' @export
#' @seealso \code{\link{get_eurostat_dsd}}.
#' @details The codelist is downloaded from Eurostat's website, through the REST API in XML (SDMX-ML) format.
#'  
#' @references For more information see the detailed documentation of the \href{https://wikis.ec.europa.eu/display/EUROSTATHELP/API+SDMX+2.1+-+metadata+query}{API}. 
#' @examples 
#' options(timeout=2)
#' get_eurostat_codelist("freq",lang="de",cache=FALSE,verbose=TRUE)
#' options(timeout=60)

get_eurostat_codelist <- function(id,
                             lang="en",
                             cache=TRUE,
                             update_cache=FALSE,
                             cache_dir=NULL,
                             compress_file=TRUE,
                             verbose=FALSE,...) {
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  dmethod<-getOption("restatapi_dmethod",get("dmethod",envir=restatapi::.restatapi_env))
  if (verbose)  {message("\nget_eurostat_codelist - API version:",get("rav",envir=restatapi::.restatapi_env))}
  tbc<-TRUE #to be continued for the next steps
  if (is.null(id)){
    warning('No concept id were provided.')
    cls<-NULL
  } else {
    cls<-cls_xml<-cc_xml<-NULL
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
    if (verbose)  {message("get_eurostat_codelist - API version:",rav)}
    if (rav!=2) {
      message("Codelist can be retrived only when the API version is equal to 2.")
      tbc=FALSE
    } 
    
    if (tbc) {
      update_cache <- update_cache | getOption("restatapi_update", FALSE)
      if ((cache) & (!update_cache)) {
        cls<-restatapi::get_eurostat_cache(paste0(id,".cls.",lang),cache_dir,verbose=verbose)
      }
      if ((!cache)|(is.null(cls))|(update_cache)){
        cls_endpoint <- paste0(eval(parse(text=paste0("cfg$QUERY_BASE_URL$'",rav,"'$ESTAT$metadata$'2.1'$codelist"))),"/ESTAT/",toupper(id))
        temp<-tempfile()
        if (verbose) {
          message("\nget_eurostat_codelist - Trying to download the codelist from: ",cls_endpoint)
          tryCatch({utils::download.file(cls_endpoint,temp,dmethod)},
                   error = function(e) {
                     message("get_eurostat_codelist - Error by the download of the codelist file:",'\n',paste(unlist(e),collapse="\n"))
                   },
                   warning = function(w) {
                     message("get_eurostat_codelist - Warning by the download of the codelist file:",'\n',paste(unlist(w),collapse="\n"))
                     tbc<-FALSE
                     cls_xml<-NULL
                   })
          if (file.size(temp)!=0 & tbc) {
            message("Trying to extract the codelist from: ",temp)
            tryCatch({cls_xml<-xml2::read_xml(temp)},
                     error = function(e) {
                       message("get_eurostat_codelist - Error during the extraction of the XML from the downloaded codelist file:",'\n',paste(unlist(e),collapse="\n"))
                       cls_xml<-NULL
                     },
                     warning = function(w) {
                       message("get_eurostat_codelist - There is warning by the extraction of the XML from the downloaded codelist file:",'\n',paste(unlist(w),collapse="\n"))
                     })
          } else {
            cls_xml<-NULL
          }
        } else {
          tryCatch({utils::download.file(cls_endpoint,temp,dmethod,quiet=TRUE)},
                   error = function(e) {
                   },
                   warning = function(w) {
                     tbc<-FALSE
                     cls_xml<-NULL
                   })
          if (file.size(temp)!=0 & tbc) {
            tryCatch({cls_xml<-xml2::read_xml(temp)},
                     error = function(e) {
                       cls_xml<-NULL
                     },
                     warning = function(w) {
                       cls_xml<-NULL
                     })
          } else {
            cls_xml<-NULL
          }
        }
        unlink(temp)
        if (!is.null(cls_xml)){
          if (verbose) {message("get_eurostat_codelist - codelist NULL:",is.null(cls))}
          # create xpath:
          xpath.lang <- sprintf("*//s:Code/c:Name[@xml:lang='%s']", lang)
          
          # parse xml:
          cls <- data.table::data.table(
            "code"=xml2::xml_attr(x=xml2::xml_find_all(x=cls_xml, xpath="*//s:Code"), attr="id"),
            "name"=xml2::xml_text(xml2::xml_find_all(x=cls_xml, xpath=xpath.lang), trim=TRUE)
          )
          

        }
        if (cache){
          pl<-restatapi::put_eurostat_cache(cls,paste0(id,".cls.",lang),update_cache,cache_dir,compress_file)
          if (verbose) {message("get_eurostat_codelist - the codelist of the ",id," concept was cached ",pl,".")}
        }  
      } else {
        # cls<-NULL
        # if (verbose) {
        #    message("get_eurostat_codelist - The cls_xml is NULL. Please check in a browser the url below. If it provides valid reponse you can try again to download the codelist.\n ",cls_endpoint)
        # }
      }
      if (!is.null(cls)){
        data.table::as.data.table(cls,stringsAsFactors=FALSE)
      }
    }
        
  }
  return(cls)
}

