#' @title Extract data values from SDMX XML 
#' @description Extracts the data values from the SDMX XML data file
#' @param xml_lf an input XML leaf with data series from an SDMX XML file to extract the value and dimension from it
#' @param keep_flags a boolean if to extract the observation status (flag) information from the XML file. The default value is \code{FALSE}
#' @param stringsAsFactors if \code{TRUE} (the default) the columns are
#'        converted to factors. If \code{FALSE} they are returned as a character.
#' @param bulk a boolean with default value \code{TRUE} if the input SDMX XML file is from the bulk download facility containing all the observations. 
#'        If the input file has pre-filtered values then the \code{FALSE} should be used.  
#' @export 
#' @details It is a sub-function to use in the \code{\link{get_eurostat_data}} and the \code{\link{get_eurostat_raw}} functions.
#' @return a data frame containing the values of an SDMX node, (the dimensions, value and optional the flag(s))
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
#' toc<-get_eurostat_toc()
#' bulk_url<-toc$downloadLink.sdmx[toc$code==id]
#' temp<-tempfile()
#' download.file(bulk_url,temp)
#' sdmx_xml<-xml2::read_xml(unzip(temp, paste0(id,".sdmx.xml")))
#' xml_leafs<-xml2::xml_find_all(sdmx_xml,".//data:Series")
#' extract_data(xml_leafs[1])
#' }
#' 

extract_data<-function(xml_lf,keep_flags=FALSE,stringsAsFactors=default.stringsAsFactors(),bulk=TRUE){
  prefix<-NULL
  if (Sys.info()[['sysname']]=='Windows'){
    xml_lf<-gsub("generic:","",xml_lf)
    xml_lf<-xml2::as_xml_document(xml_lf)
  } else {
    prefix<-"generic:"
  }
  if(bulk){
    bd<-t(as.data.frame(xml2::xml_attrs(xml_lf)))
    rownames(bd)<-NULL
    dv<-xml2::xml_attrs(xml2::xml_children(xml_lf))
    if (keep_flags){
      cn<-c("TIME_PERIOD","OBS_VALUE","OBS_STATUS")
    } else {
      cn<-c("TIME_PERIOD","OBS_VALUE")
    }
    df<-do.call(rbind, lapply(lapply(dv, unlist),"[", cn))
    colnames(df)<-cn
    out<-data.frame(bd,df,stringsAsFactors=stringsAsFactors)  
  } else {
    tmp<-as.data.frame(xml2::xml_attrs(xml2::xml_children(xml2::xml_find_all(xml_lf,paste0(".//",prefix,"SeriesKey")))),stringsAsFactors=FALSE)
    bd<-as.data.frame(tmp[2,],stringsAsFactors=stringsAsFactors)
    colnames(bd)<-tmp[1,]
    rownames(bd)<-NULL
    obs<-xml2::xml_find_all(xml_lf,paste0(".//",prefix,"Obs"))
    cn<-c("obsTime","obsValue") 
    df<-data.table::rbindlist(lapply(obs, function(x) {dr<-as.data.frame(t(unlist(xml2::xml_attrs(xml2::xml_children(x),"value"))),stringsAsFactors=FALSE)
                                                rownames(dr)<-NULL
                                                colnames(dr)<-cn
                                                if ((keep_flags)){
                                                  f<-xml2::xml_attr(xml2::xml_children(xml2::xml_children(x)),"value")
                                                  if(length(f)==0){
                                                    dr$OBS_STATUS<-""
                                                  } else{
                                                    if (f=="na"){
                                                      dr$OBS_STATUS<-""
                                                    } else{
                                                      dr$OBS_STATUS<-f[f!="na"]
                                                    }
                                                  }
                                                }
                                                return(as.data.frame(dr))
      }), fill=TRUE)
    df$obsValue<-as.numeric(df$obsValue)
    df$obsValue[df$obsValue=="NaN"]<-NA
    if (keep_flags){
      out<-data.frame(bd,df,stringsAsFactors=stringsAsFactors)  
    } else{
      out<-data.frame(bd,df[,1:2],stringsAsFactors=stringsAsFactors)
    }
  }
  return(out)
}