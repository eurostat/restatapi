#' @title Load configuration data from JSON
#' @description Load the configuration information to the '.restatapi_env' from the JSON configuration file.
#' @param api_version  It can be either "old", "new", "test" or "current". The default value is "current".
#' @param verbose  A boolean if the verbose message about the configuration to be showed or not. The default is \code{FALSE}. 
#' @return it returns 2 objects in the '.restatapi_env'  
#' \itemize{
#'  \item \code{cfg} a list with all the configuration data
#'  \item \code{rav} a character string with a number defining the API_VERSION from the config file to be used later. It is determined based on the \code{api_version} parameter.   
#'  } 
#' @export
#' @details Loads configuration data from a JSON file. By this way different version of the API can be tested.
#' @examples 
#' \donttest{
#' load_cfg()
#' load_cfg(api_version="test",verbose=TRUE)
#' }
#' 

load_cfg<-function(api_version="current", verbose=FALSE){
  assign(".restatapi_env", new.env(), envir=baseenv())
  assign("cfg", rjson::fromJSON(file =system.file("extdata", "rest_api_config.json", package = "restatapi")), envir = .restatapi_env)
  cfg<-get("cfg", envir = .restatapi_env)
  assign("rav",eval(parse(text=paste0("cfg$API_VERSIONING$",api_version))), envir = .restatapi_env)
  rav<-get("rav", envir = .restatapi_env)
  if (verbose) {message("restatapi: The 'current' API version number is ",cfg$API_VERSIONING$current," and ",getOption("mc.cores"), if (getOption("mc.cores")>1) {" cores are"} else {" core is"}, " used for parallel computing.\n           Loaded config file with the API version number ",rav,"\n")}
}