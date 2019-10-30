#' @title Download the Table of Contents of Eurostat datasets
#' @description Download Table of Contents (TOC) of Eurostat datasets if it is not cached previously. 
#' @param mode a character string either \code{xml} or \code{txt} defining the download mode. Depending on the mode the 'xml' version or the 'text' version of the TOC is downloaded.
#' The deafult value is \code{xml} as it provides more information (e.g. number of values, short description and  download links in diffrent format) 
#' @param cache a boolean whether to load/save the TOC from/in the cache or not. The default value is \code{TRUE}, so that the TOC is checked first in the cache and if does not exist then downloaded from Eurostat and cached.
#' @param update_cache a boolean to update cache or not. The default value is \code{FALSE}, so the cache is not updated. Can be set also with \code{options(restatapi_update=TRUE)}
#' @param cache_dir a path to a cache directory. The default is \code{NULL}, in this case the TOC is cached in the memory (in the '.restatapi_env'). Otherwise if the \code{cache_dir} directory does not exist it creates the 'restatapi' directory in the temporary directory from \code{tempdir()} to save the RDS- file. Directory can also be set with \code{option(restatapi_cache_dir=...)}.
#' @param compress_file a logical whether to compress the RDS-file in caching. Default is \code{TRUE}.
#' @param lang a character string either \code{en}, \code{de} or \code{fr} to define the language version for the table of contents. The default is \code{en} - English.
#' @param verbose A boolean with default \code{FALSE}, so detailed messages (for debugging) will not printed.
#'         Can be set also with \code{options(restatapi_verbose=TRUE)}
#' @param ... parameter to pass on the \code{load_cfg} function
#' @return A table with the following columns:
#'    \tabular{ll}{
#'      \code{title} \tab The name of dataset/table in the language provided by the \code{lang} parameter \cr
#'      \code{code} \tab The codename of dataset/table which can be used by the \code{get_eurostat} function \cr
#'      \code{type} \tab The type of information: 'dataset' or 'table' \cr
#'      \code{lastUpdate} \tab The date when the data was last time updated for tables and datasets\cr
#'      \code{lastModified}\tab The date when the structure of the dataset/table was last time modified\cr
#'      \code{dataStart}\tab The start date of the data in the dataset/table\cr
#'      \code{dataEnd}\tab The end date of the data in the dataset/table\cr
#'      \code{values}\tab The number of values in the dataset/table. It is filled only if the download \code{mode} is "xml"\cr
#'      \code{unit}\tab The unit name for tables in the language provided by the \code{lang} parameter. For dataset it is empty. The column exists only if the download \code{mode} is "xml"\cr
#'      \code{shortDescription}\tab The short description of the values for tables in the language provided by the \code{lang} parameter. For dataset it is empty. The column exists only if the download \code{mode} is "xml"\cr
#'      \code{metadata.html}\tab The link to the metadata in html format. The column exists only if the download \code{mode} is "xml"\cr
#'      \code{metadata.sdmx}\tab The link to the metadata in SDMX format. The column exists only if the download \code{mode} is "xml"\cr
#'      \code{downloadLink.tsv}\tab The link to the whole dataset/table in tab separated values format in the bulk download facility. The column exists only if the download \code{mode} is "xml"\cr
#'      \code{downloadLink.sdmx}\tab The link to the whole dataset/table in SDMX format in the bulk download facility. The column exists only if the download \code{mode} is "xml"
#'    }
#' @export
#' @seealso \code{\link{get_eurostat_data}}, \code{\link{search_eurostat_toc}}.
#' @details The TOC is downloaded from Eurostat websites through the REST API for the \code{xml} (default) version or from the bulk download facilities for \code{txt} version.
#'  
#' @references For more technical information see the detailed documentation of the \href{https://ec.europa.eu/eurostat/data/web-services}{API}. 
#' @examples 
#' \dontshow{
#' if (parallel::detectCores()<=2){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }    
#' }
#' \donttest{
#' toc_xml<-get_eurostat_toc(cache=FALSE,verbose=TRUE)
#' head(toc_xml)
#' toc_txt<-get_eurostat_toc(mode="txt", lang="de")
#' head(toc_txt)
#' }

get_eurostat_toc<-function(mode="xml",
                           cache=TRUE,
                           update_cache=FALSE,
                           cache_dir=NULL,
                           compress_file=TRUE,
                           lang="en",
                           verbose=FALSE,...) {
  toc<-xml_leafs<-NULL
  ne<-TRUE
  if ((!exists(".restatapi_env"))|(length(list(...))>0)) {load_cfg(...)}
  update_cache<-update_cache|getOption("restatapi_update",FALSE)
  if(any(grepl("get_eurostat_bulk|get_eurostat_data|get_eurostat_raw",as.character(sys.calls()),perl=TRUE))) {update_cache<-FALSE}
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  if ((cache) & (!update_cache)) {
    toc<-get_eurostat_cache(paste0("toc.",mode,".",lang),cache_dir,verbose=verbose)
  }
  if ((!cache)|(is.null(toc))|(update_cache)){
    cfg<-get("cfg",envir=.restatapi_env) 
    rav<-get("rav",envir=.restatapi_env)
    method<-get("dmethod",envir=.restatapi_env)
    if(mode=="txt"){
      toc_endpoint<-eval(parse(text=paste0("cfg$TOC_ENDPOINT$'",rav,"'$ESTAT$txt$",lang)))
      temp<-tempfile()
      if (verbose) {
        message("Downloading ",toc_endpoint)
        tryCatch({utils::download.file(toc_endpoint,temp,method)},
                 error = function(e) {
                   message("Error during the download of the tsv version of the TOC file:",'\n',paste(unlist(e),collapse="\n"))
                   ne<-FALSE
                 },
                 warning = function(w) {
                   message("Warning by the download of the tsv version of the TOC file:",'\n',paste(unlist(w),collapse="\n"))
                 })
      } else {
        tryCatch({utils::download.file(toc_endpoint,temp,method,quiet=TRUE)},
                 error = function(e) {ne<-FALSE},
                 warning = function(w) {})
      }
      if (ne) {
        toc<-utils::read.csv(temp,header=TRUE,sep="\t",stringsAsFactors=FALSE)
        names(toc)<-c("title","code","type","lastUpdate","lastModified","dataStart","dataEnd","values")
        toc<-toc[toc$type!="folder",]
        toc$title<-sub("^\\s*","",toc$title)
        unlink(temp)
      }  
    } else if (mode=="xml"){
      toc_endpoint<-eval(parse(text=paste0("cfg$TOC_ENDPOINT$'",rav,"'$ESTAT$xml")))
      if (verbose) {
        message("Downloading ",toc_endpoint)
        tryCatch({xml_leafs<-xml2::xml_find_all(xml2::read_xml(toc_endpoint),".//nt:leaf")},
                 error = function(e) {
                   message("Error during the download of the xml version of the TOC file:",'\n',paste(unlist(e),collapse="\n"))
                   ne<-FALSE
                 },
                 warning = function(w) {
                   message("Warning by the download of the xml version of the TOC file:",'\n',paste(unlist(w),collapse="\n"))
                 })
      } else {
        tryCatch({xml_leafs<-xml2::xml_find_all(xml2::read_xml(toc_endpoint),".//nt:leaf")},
                 error = function(e) {ne<-FALSE},
                 warning = function(w) {})
      }
      if ((ne)){
        if (!is.null(xml_leafs)){
          if (length(xml_leafs)>0){
            if (Sys.info()[['sysname']]=='Windows'){
              cl<-parallel::makeCluster(getOption("restatapi_cores",1L))
              parallel::clusterEvalQ(cl,require(xml2))
              parallel::clusterExport(cl,c("extract_toc"))
              leafs<-parallel::parLapply(cl,as.character(xml_leafs),extract_toc)
              parallel::stopCluster(cl)
            }else{
              leafs<-parallel::mclapply(xml_leafs,extract_toc,mc.cores=getOption("restatapi_cores",1L))
            }
            toc<-data.frame(t(sapply(leafs, '[', seq(max(lengths(leafs))))),stringsAsFactors=FALSE)
            type<-as.character(unlist(lapply(xml_leafs,xml2::xml_attrs)))
            toc<-cbind(toc,type)
            names(toc)<-c(sub("\\.$","",paste(xml2::xml_name(xml2::xml_children(xml_leafs[1])),sub(".*)","",as.character(xml2::xml_attrs(xml2::xml_children(xml_leafs[1])))),sep="."),perl=TRUE),"type")
            toc<-toc[,c(paste0("title.",lang),"code","type","lastUpdate","lastModified","dataStart","dataEnd","values",paste0("unit.",lang),paste0("shortDescription.",lang),"metadata.html","metadata.sdmx","downloadLink.tsv","downloadLink.sdmx")]
            names(toc)<-c("title","code","type","lastUpdate","lastModified","dataStart","dataEnd","values","unit","shortDescription","metadata.html","metadata.sdmx","downloadLink.tsv","downloadLink.sdmx")        
          } 
        }
      }
    } else {
      stop('Incorrect mode is given. It should be either "xml" or "txt".')
    } 
    if (!is.null(toc)){
      toc<-toc[!duplicated(toc[,c(1:8)]),]
      toc$values<-as.numeric(toc$values)
      toc$lastUpdate<-as.Date(toc$lastUpdate,"%d.%m.%Y")
      toc$lastModified<-as.Date(toc$lastModified,"%d.%m.%Y")  
    } else{
      if (verbose) {message("The TOC is empty. Please check the download link form the line above in a web browser.")}
    }
  }  
  if (!is.null(toc)&cache){
    name<-paste0("toc.",mode,".",lang)
    pl<-put_eurostat_cache(toc,name,update_cache,cache_dir,compress_file)
    if (verbose){message("The TOC was cached ",pl,".\n")}
  }
  return(toc)  
}