#' @title Load configuration data from JSON
#' @description Load the configuration information to the '.restatapi_env' from the JSON configuration file.
#' @param api_version  It can be either "old", "new", "test" or "current". The default value is "current".
#' @param parallel A boolean with the default value \code{TRUE}. If the system is not Windows and there are multiple cores/logical processors then part of the data 
#'        extraction is made in parallel reducing significantly the time needed. If the value is \code{FALSE} the option \code{restatapi_cores} set to 1.
#' @param max_cores A boolean with the default value \code{TRUE}. If the parameter 'parallel' is \code{TRUE} then this parameter is taken into account otherwise it is ignored.
#'        The default value \code{TRUE}, in this case the maximum minus one cores/logical processors are used for parallel computing. If the parameter \code{FALSE}, 
#'        then the default value of \code{getOption("mc.cores")} is used, if it is defined. If \code{mc.cores} is \code{NULL} and there are more than 2 cores/logical processors then the option \code{restatapi_cores} are set to 2. 
#'        Otherwise the parallel processing turned off by setting the option \code{restatapi_cores} to 1.
#'        The number of cores used for parallel computing can be changed any time with \code{options(restatapi_cores=...)}     
#' @param verbose  A boolean if the verbose message about the configuration to be showed or not. The default is \code{FALSE}. Can be set also with \code{options(restatapi_verbose=TRUE)} 
#' @return it returns 2 objects in the '.restatapi_env'  
#' \itemize{
#'  \item \code{cfg} a list with all the configuration data
#'  \item \code{rav} a character string with a number defining the API_VERSION from the config file to be used later. It is determined based on the \code{api_version} parameter.   
#'  } 
#' @export
#' @details Loads configuration data from a JSON file. The function first tries to load the configuration file from GitHub. 
#'          If it is not possible it loads from  By this way different version of the API can be tested. In addition, 
#'          the list of country codes are loaded to the variable \code{cc} (country codes), based on the  \href{https://ec.europa.eu/eurostat/ramon/nomenclatures/index.cfm?TargetUrl=LST_NOM_DTL&StrNom=CL_GEO&StrLanguageCode=EN&IntPcKey=42277583&IntResult=1&StrLayoutCode=HIERARCHIC}{Eurostat standard code list}
#' @examples 
#' \donttest{
#' load_cfg()
#' load_cfg(parallel=FALSE)
#' load_cfg(api_version="test",verbose=TRUE,max_cores=FALSE)
#' }
#' \dontshow{
#' load_cfg()
#' eu<-get("cc",envir=.restatapi_env)
#' eu$EU28
#' eu$EA15
#' }
#' 

load_cfg<-function(api_version="current",parallel=TRUE,max_cores=TRUE,verbose=FALSE){
  verbose<-verbose|getOption("restatapi_verbose",FALSE)
  assign(".restatapi_env",new.env(),envir=baseenv())
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
  if (capabilities("libcurl")){
    assign("dmethod","libcurl",envir=.restatapi_env)
  }else{
    assign("dmethod","auto",envir=.restatapi_env)
  }
  if (parallel) {
    if (max_cores){
      options(restatapi_cores=parallel::detectCores()-1)
    } else {
      if (max(getOption("mc.cores"),Sys.getenv("MC_CORES"))>0){
        options(restatapi_cores=max(getOption("mc.cores"),Sys.getenv("MC_CORES")))
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
    parallel_text<-paste0(getOption("restatapi_cores")," from the ",parallel::detectCores()," cores are used for parallel computing.")    
  }
  if (verbose) {message("restatapi: - config file with the API version ",rav," loaded from ",cfg_source," (the 'current' API version number is ",cfg$API_VERSIONING$current,").\n           - ",parallel_text,"\n           - '",get("dmethod",envir=.restatapi_env),"' will be used for file download.")}
}
