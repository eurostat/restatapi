#' @title Extract data values from SDMX XML 
#' @description Extracts the data values from the SDMX XML data file
#' @param xml_lf an input XML leaf with data series from an SDMX XML file to extract the value and its dimensions from it
#' @param keep_flags a logical value if to extract the observation status (flag) information from the XML file. The default value is \code{FALSE}
#' @param stringsAsFactors a logical value. If \code{TRUE} the columns are converted to factors. The default is \code{FALSE}, 
#'        in this case the strings are returned as characters.
#' @param bulk a logical value with default value \code{TRUE} if the input SDMX XML file is from the bulk download facility containing all the observations. 
#'        If the input file has pre-filtered values then the value \code{FALSE} should be used.  
#' @param check_toc if the data file was downloaded using the URL from the TOC or not. The default is FALSE means not the TOC link is used.         
#' @export 
#' @details It is a sub-function to use in the \code{\link{get_eurostat_data}} and the \code{\link{get_eurostat_raw}} functions.
#' @return a data frame containing the values of an SDMX node: the dimensions, value and the optional flag(s)
#' @examples 
#' \dontshow{
#' if (parallel::detectCores()<=2){
#'    options(restatapi_cores=1)
#' }else{
#'    options(restatapi_cores=2)
#' }
#' }
#' \donttest{
#' id<-"agr_r_milkpr"
#' url<-paste0("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
#'             id,
#'             "?format=sdmx_2.1_structured&compressed=true")
#' if (!(grepl("amzn|-aws|-azure ",Sys.info()['release']))) options(timeout=2)
#'   sdmx_xml<-get_compressed_sdmx(url,verbose=TRUE)
#'   xml_leafs<-xml2::xml_find_all(sdmx_xml,".//Series")
#'   extract_data(xml_leafs[1])
#' options(timeout=60)
#' }
#' 

extract_data<-function(xml_lf,keep_flags=FALSE,stringsAsFactors=FALSE,bulk=TRUE,check_toc=FALSE){
  # if (getOption("restatapi_verbose",FALSE))  {message("extract_data - API version:",get("rav",envir=restatapi::.restatapi_env))}
  rav<-get("rav",envir=restatapi::.restatapi_env)
  prefix<-switch(rav,"1"="generic:","2"="g:")
  if(bulk){
    if (Sys.info()[['sysname']]=='Windows'){
      xml_lf<-gsub(prefix,"",xml_lf)
      xml_lf<-xml2::as_xml_document(xml_lf)
    } 
    bd<-t(as.data.frame(xml2::xml_attrs(xml_lf),stringsAsFactors=FALSE))
    rownames(bd)<-NULL
    dv<-xml2::xml_attrs(xml2::xml_children(xml_lf))
    if (keep_flags){
      flagc<-switch(rav,"1"="OBS_STATUS","2"="OBS_FLAG")
      # if (check_toc) {flagc<-"OBS_STATUS"}
      cn<-c("TIME_PERIOD","OBS_VALUE",flagc)
    } else {
      cn<-c("TIME_PERIOD","OBS_VALUE")
    }
    df<-do.call(rbind, lapply(lapply(dv, unlist),"[", cn))
    colnames(df)<-cn
    out<-data.frame(bd,df,stringsAsFactors=FALSE)  
  } else {
    # if (Sys.info()[['sysname']]=='Windows'){
    #   xml_lf<-xml2::as_xml_document(xml_lf)
    # } 
    tmp<-as.data.frame(xml2::xml_attrs(xml2::xml_children(xml2::xml_find_all(xml_lf,paste0(".//",prefix,"SeriesKey")))),stringsAsFactors=FALSE)
    bd<-as.data.frame(tmp[2,],stringsAsFactors=FALSE)
    colnames(bd)<-tmp[1,]
    rownames(bd)<-NULL
    obs<-xml2::xml_find_all(xml_lf,paste0(".//",prefix,"Obs"))
    cn<-c("obsTime","obsValue") 
    df<-data.table::rbindlist(lapply(obs, function(x) {dr<-as.data.frame(t(unlist(xml2::xml_attrs(xml2::xml_children(x)))),stringsAsFactors=FALSE)
                                                rownames(dr)<-NULL
                                                if (ncol(dr)<2) {dr$nc<-""}
                                                colnames(dr)<-cn
                                                if ((keep_flags)){
                                                  f<-xml2::xml_attr(xml2::xml_children(xml2::xml_children(x)),"value")
                                                  if(length(f)==0){
                                                    dr$OBS_STATUS<-""
                                                  } else{
                                                    f<-gsub("na","",f)
                                                    dr$OBS_STATUS<-paste0(f,collapse="")
                                                  }
                                                }
                                                return(as.data.frame(dr,stringsAsFactors=FALSE))
      }), fill=TRUE)
    if (any(!grepl("\\d:\\d",df$obsValue))) df$obsValue<-as.numeric(df$obsValue)
    # df$obsValue<-as.numeric(df$obsValue)
    df$obsValue[df$obsValue=="NaN"]<-NA
    if (keep_flags){
      out<-data.frame(bd,df,stringsAsFactors=stringsAsFactors)  
    } else{
      out<-data.frame(bd,df[,1:2],stringsAsFactors=stringsAsFactors)
    }
  }
  return(out)
}
