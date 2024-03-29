% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_eurostat_raw.R
\name{get_eurostat_raw}
\alias{get_eurostat_raw}
\title{Get Eurostat data as it is}
\usage{
get_eurostat_raw(
  id,
  mode = "txt",
  cache = TRUE,
  update_cache = FALSE,
  cache_dir = NULL,
  compress_file = TRUE,
  stringsAsFactors = FALSE,
  keep_flags = FALSE,
  check_toc = FALSE,
  melt = TRUE,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{id}{A code name for the dataset of interest.
See \code{\link{search_eurostat_toc}} for details how to get an id.}

\item{mode}{defines the format of the downloaded dataset. It can be \code{txt} (the default value) for 
Tab Separated Values (TSV), or \code{csv} for SDMX-CSV, or \code{xml} for the SDMX-ML version.}

\item{cache}{a logical whether to do caching. Default is \code{TRUE}.}

\item{update_cache}{a logical with a default value \code{FALSE}, whether to update cache. Can be set also with
\code{options(restatapi_update=TRUE)}}

\item{cache_dir}{a path to a cache directory. The \code{NULL} (default) uses the memory as cache. 
If the folder  if the \code{cache_dir} directory does not exist it saves in the 'restatapi' directory 
under the temporary directory from \code{tempdir()}. Directory can also be set with
\code{option(restatapi_cache_dir=...)}.}

\item{compress_file}{a logical whether to compress the
RDS-file in caching. Default is \code{TRUE}.}

\item{stringsAsFactors}{if \code{TRUE} the variables which are not numeric are
converted to factors. The default value \code{FALSE}, in this case they are returned as characters.}

\item{keep_flags}{a logical whether the observation status (flags) - e.g. "confidential",
"provisional", etc. - should be kept in a separate column or if they
can be removed. Default is \code{FALSE}. For flag values see: 
\url{https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/codelist/ESTAT/OBS_STATUS/?compressed=false&format=TSV&lang=en}.}

\item{check_toc}{a boolean whether to check the provided \code{id} in the Table of Contents (TOC) or not. The default value 
\code{FALSE}, in this case the base URL for the download link is retrieved from the configuration file. 
If the value is \code{TRUE} then the TOC is downloaded and the \code{id} is checked in it. If it found then the download link 
is retrieved form the TOC.}

\item{melt}{a boolean with default value \code{TRUE} and used only if the \code{mode="txt"}. In case it is \code{FALSE}, 
the downloaded tsv file is not melted, the time dimension remains in columns and it does not process the flags.}

\item{verbose}{A boolean with default \code{FALSE}, so detailed messages (for debugging) will not printed.
Can be set also with \code{options(restatapi_verbose=TRUE)}}

\item{...}{further argument for the \code{\link{load_cfg}} function}
}
\value{
a data.table with the following columns if the default \code{melt=TRUE} is used:
 \tabular{ll}{
 \code{FREQ} \tab The frequency of the data (\strong{A}nnual, \strong{S}emi-annual, \strong{H}alf-year, \strong{Q}uarterly, \strong{M}onthly,
 \strong{W}eekly, \strong{D}aily)\cr
 dimension names \tab One column for each dimension in the data\cr
 \code{TIME_FORMAT} \tab A column for the time format, if the source file SDMX-ML and the data was not
 loaded from a previously cached TSV download (this column is missing if the source file is TSV)\cr
 \code{time/TIME_PERIOD} \tab A column for the time dimension, where the name of the column depends on the
 source file (TSV/SDMX-ML)\cr
 \code{values/OBS_VALUE} \tab A column for numerical values, where the name of the column depends on the
 source file (TSV/SDMX-ML)\cr
 \code{flags/OBS_STATUS} \tab A column for flags if the \code{keep_flags=TRUE} otherwise this column is not included
 in the data table, and the name of the column depends on the source file (TSV/SDMX-ML)
 }
The data does not include all missing values. The missing values are dropped if the value and flags are missing
on a particular time. 

In case \code{melt=FALSE} the results is a data.table where the first column contains the comma separated values of the various dimensions, and the columns contains the observations for each time dimension.
}
\description{
Download data sets from \href{https://ec.europa.eu/eurostat}{Eurostat} database .
}
\details{
Data sets are downloaded from \href{https://wikis.ec.europa.eu/display/EUROSTATHELP/Transition+-+from+Eurostat+Bulk+Download+to+API}{the Eurostat bulk download facility} 
in CSV, TSV or SDMX format.


The \code{id}, should be a value from the \code{code} column of the table of contents (\code{\link{get_eurostat_toc}}), and can be searched for with the \code{\link{search_eurostat_toc}} function. The id value can be retrieved from the \href{https://ec.europa.eu/eurostat/data/database}{Eurostat database}
 as well. The Eurostat database gives codes in the Data Navigation Tree after every dataset in parenthesis.
By default all datasets downloaded in TSV format and cached as they are often rather large. 
The datasets cached in memory (default) or can be stored in a temporary directory if \code{cache_dir} or \code{option(restatpi_cache_dir)} is defined.
The cache can be emptied with \code{\link{clean_restatapi_cache}}.
If the \code{id} is checked in TOC then the data will saved in the cache with the date from the "lastUpdate" column from the TOC, otherwise it is saved with the current date.
}
\examples{
\dontshow{
if (parallel::detectCores()<=2){
   options(restatapi_cores=1)
}else{
   options(restatapi_cores=2)
}    
}
\donttest{
if (!(grepl("amzn|-aws|-azure ",Sys.info()['release']))) options(timeout=2)
head(get_eurostat_raw("agr_r_milkpr",keep_flags=TRUE))
head(get_eurostat_raw("avia_par_ee",mode="xml",check_toc=TRUE,update_cache=TRUE,verbose=TRUE))
options(restatapi_update=FALSE)
head(get_eurostat_raw("avia_par_me",mode="txt",melt=FALSE))
head(get_eurostat_raw("avia_par_me",
                      mode="txt",
                      cache_dir=tempdir(),
                      compress_file=FALSE,
                      verbose=TRUE))
options(timeout=60)
}
}
\seealso{
\code{\link{get_eurostat_data}}, \code{\link{get_eurostat_bulk}}
}
