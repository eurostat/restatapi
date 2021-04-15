#' @title Load configuration data from JSON
#' @description Load the configuration information to the '.restatapi_env' from the JSON configuration file.
#' @param api_version  It can be either "old", "new", "test" or "current". The default value is "current".
#' @param load_toc The default value \code{FALSE}, which means that the XML version of the Table of contents (TOC) will not be downloaded and 
#'        cached automatically in the '.restatapi_env' when the package is loaded.
#' @param parallel A boolean with the default value \code{TRUE}. If there are multiple cores/logical processors then part of the data 
#'        extraction is made in parallel reducing significantly the time needed for large datasets. If the value is \code{FALSE} the option \code{restatapi_cores} set to 1.
#' @param max_cores A boolean with the default value \code{FALSE}. If the parameter 'parallel' is \code{TRUE} then this parameter is taken into account otherwise it is ignored.
#'        If the value is \code{TRUE}, then the maximum minus one cores/logical processors are used for parallel computing. If the parameter \code{FALSE}, 
#'        then the default value of \code{getOption("mc.cores")} is used, if it is defined. If \code{mc.cores} is \code{NULL} then depending on the memory size and number of available cores/threads the \code{restatapi_cores} are set to 2 or 4 cores/logical processors. 
#'        Otherwise the parallel processing turned off by setting the option \code{restatapi_cores} to 1.
#'        The number of cores used for parallel computing can be changed any time with \code{options(restatapi_cores=...)}     
#' @param verbose  A boolean if the verbose message about the configuration to be showed or not. The default is \code{FALSE}. Can be set also with \code{options(restatapi_verbose=TRUE)} 
#' @return it returns 4 objects in the '.restatapi_env'  
#' \itemize{
#'  \item \code{cfg} a list with all the configuration data
#'  \item \code{rav} a character string with a number defining the API_VERSION from the configuration file to be used later. It is 
#'              determined based on the \code{api_version} parameter.   
#'  \item \code{cc} a list containing the 2 character country codes of the member states for different EU composition like EU15, EU28 or EA (Euro Area).   
#'  \item \code{dmethod} the download method to be used to access Eurostat database. If the 'libcurl' method exists under Windows then 
#'              it will be the default method for file download, otherwise it will be set 'auto'. The download method can be changed any time with \code{options(restatapi_dmethod=...)}
#'  } 
#' @export
#' @details Loads configuration data from a JSON file. The function first tries to load the configuration file from GitHub. 
#'          If it is not possible it loads from the file delivered with the package. By this way different version of the API can be tested. 
#'          Since in many cases there is http/https redirection in the download which can cause problems with the 'wininet' download method, the 'libcurl' method is used when it is available.
#'          This configuration code sets up the parallel processing to handle large XML files efficiently. By default if there is more then 4 cores/logical processors and at least 32 GB of RAM then
#'          4 cores are used for parallel computing. If there is more then 2 cores then 2 cores are used. This default configuration can be overwritten with \code{options(restatapi_cores=...)} or with the \code{max_cores=TRUE} parameter.
#'          In the second case part of the computation distributed over the maximum number minus one cores. By using the \code{max_cores=TRUE} option there is a higher probability that the program will run out off memory for larger datasets.  
#'          In addition, the list of country codes are loaded to the variable \code{cc} (country codes), based on the  \href{https://ec.europa.eu/eurostat/ramon/nomenclatures/index.cfm?TargetUrl=LST_NOM_DTL&StrNom=CL_GEO&StrLanguageCode=EN&IntPcKey=42277583&IntResult=1&StrLayoutCode=HIERARCHIC}{Eurostat standard code list}
#' @examples 
#' \donttest{
#' load_cfg(parallel=FALSE)
#' load_cfg(api_version="test",verbose=TRUE,max_cores=FALSE)
#' load_cfg()
#' eu<-get("cc",envir=.restatapi_env)
#' eu$EU28
#' eu$EA15
#' }
#' 

load_cfg<-function(api_version="current",load_toc=FALSE,parallel=TRUE,max_cores=FALSE,verbose=FALSE){
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  options(restatapi_log=TRUE)
  # .restatapi_env<-new.env()
  # assign(".restatapi_env",new.env(),envir=parent.env(parent.frame()))
  cfg_source<-"GitHub"
  tryCatch(
    {assign("cfg",rjson::fromJSON(file="https://raw.githubusercontent.com/eurostat/restatapi/master/inst/extdata/rest_api_config.json"),envir=.restatapi_env)},
    error = function(e) 
      {if (verbose) {warning("The configuration file could not be downloaded from GitHub, the preinstalled file in the package is used.")}
      assign("cfg",rjson::fromJSON(file=system.file("extdata","rest_api_config.json",package="restatapi")),envir=.restatapi_env)
      cfg_source<-"the file installed locally"})
  cfg<-get("cfg",envir=.restatapi_env)
  assign("rav",eval(parse(text=paste0("cfg$API_VERSIONING$",api_version))),envir=.restatapi_env)
  rav<-get("rav",envir=.restatapi_env)
  assign("cc",cfg$COUNTRIES,envir=.restatapi_env)
  
  if ((capabilities("libcurl")) & (Sys.info()[['sysname']]=='Windows')){
    options(restatapi_dmethod="libcurl")
    assign("dmethod","libcurl",envir=.restatapi_env)
  }else{
    options(restatapi_dmethod="auto")
    assign("dmethod","auto",envir=.restatapi_env)
  }
  

  if (load_toc){
    options(restatapi_cores=1)
    toc<-get_eurostat_toc(verbose=FALSE)
    if (!is.null(toc)){
      assign("toc.xml",toc,envir=.restatapi_env)
      msg_end<-"\n           - the Table of contents (TOC) successfully cached in '.restatapi_env'."
    } else{
      msg_end<-"\n           - the download and caching of the Table of contents (TOC) were unsuccessful."
    }
  } else{
    msg_end<-"\n           - the Table of contents (TOC) was not pre-loaded into the deafult cache ('.restatapi_env')."
  }
  

  suppressWarnings(mem_size<-switch(Sys.info()[['sysname']],
                   Windows={tryCatch({as.numeric(gsub("[^0-9]","",system("wmic MemoryChip get Capacity", intern = TRUE)[2]))/1024/1024},error=function(e){0},warning=function(w){0})},
                   # Darwin={tryCatch({as.numeric(substring(system('system_profiler SPHardwareDataType | grep "  Memory:"', intern = TRUE), 13))},error=function(e){0},warning=function(w){0})},
                   # SunOS={tryCatch({as.numeric(gsub("[^0-9]","",system("prtconf | grep Memory", intern = TRUE,ignore.stderr=TRUE)))},error=function(e){0},warning=function(w){0})},
                   Linux={tryCatch({as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo",intern=TRUE,ignore.stderr=TRUE))/1024},error=function(e){0},warning=function(w){0})}
                  ))
  if (is.null(mem_size)|length(mem_size)==0){mem_size<-0}
  if (parallel) {
    if (max_cores){
      options(restatapi_cores=parallel::detectCores()-1)
    } else {
      if (max(getOption("mc.cores"),Sys.getenv("MC_CORES"))>0){
        options(restatapi_cores=max(getOption("mc.cores"),Sys.getenv("MC_CORES")))
      } else if (parallel::detectCores()>4){
        if (mem_size>30000){
          options(restatapi_cores=4)
        } else {
          options(restatapi_cores=2)  
        }
      } else if (parallel::detectCores()>2){
        options(restatapi_cores=2)  
      } else {
        options(restatapi_cores=1)
      }
    }
  }
  
  if (getOption("restatapi_cores")<2){
    parallel_text<-"no parallel computing."
  } else{
    parallel_text<-paste0(getOption("restatapi_cores")," from the ",parallel::detectCores()," cores are used for parallel computing, can be changed with 'options(restatapi_cores=...)'")    
  }
  
  if (verbose) {message("restatapi: - config file with the API version ",rav," loaded from ",cfg_source," (the 'current' API version number is ",cfg$API_VERSIONING$current,").\n           - ",parallel_text,"\n           - '",getOption("restatapi_dmethod","auto"),"' method will be used for file download, can be changed with 'options(restatapi_dmethod=...)'",msg_end)}
}
