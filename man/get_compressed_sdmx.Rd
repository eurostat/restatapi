% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_compressed_sdmx.R
\name{get_compressed_sdmx}
\alias{get_compressed_sdmx}
\title{Download and extract compressed SDMX XML}
\usage{
get_compressed_sdmx(url = NULL, verbose = FALSE, format = "gz")
}
\arguments{
\item{url}{a URL from the bulk download facility to download the zipped SDMX XML file}

\item{verbose}{a logical value with default \code{FALSE}, so detailed messages (for debugging) will not printed.
Can be set also with \code{options(restatapi_verbose=TRUE)}.}

\item{format}{the format of the compression, either "zip" or "gz" the default value}
}
\value{
an xml class object with SDMX tags extracted and read from the downloaded file.
}
\description{
Downloads  and extracts the data values from the SDMX XML data file
}
\details{
It is a sub-function to use in the \code{\link{get_eurostat_raw}} and the \code{\link{get_eurostat_data}} functions.
}
\examples{
id<-"agr_r_milkpr"
url<-paste0("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
            id,
            "?format=sdmx_2.1_structured&compressed=true")
if (!(grepl("amzn|-aws|-azure ",Sys.info()['release']))) options(timeout=2)
sdmx_xml<-get_compressed_sdmx(url,verbose=TRUE,format="gz")
options(timeout=60)
}
