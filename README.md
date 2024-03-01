<!-- badges: start -->
[![last commit](https://img.shields.io/github/last-commit/eurostat/restatapi?style=flat)](https://github.com/eurostat/restatapi/commits/)
[![R build
status](https://github.com/eurostat/restatapi/workflows/R-CMD-check/badge.svg)](https://github.com/eurostat/restatapi/actions)
[![dependencies](https://tinyverse.netlify.com/badge/restatapi)](https://CRAN.R-project.org/package=restatapi)
[![CRAN version](https://www.r-pkg.org/badges/version/restatapi)](https://CRAN.R-project.org/package=restatapi )
[![CRAN status](https://badges.cranchecks.info/summary/restatapi.svg)](https://cran.r-project.org/web/checks/check_results_restatapi.html)
[![license](https://img.shields.io/badge/license-EUPL-success)](https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12)
[![weekly downloads](https://cranlogs.r-pkg.org/badges/last-week/restatapi)](https://mybinder.org/v2/gh/mmatyi/restatapi_logs/b1320a7cd483638e1f12c8a1f5bf595cbbc32233?urlpath=shiny/ShinyApps/cran_stat/)
[![monthly downloads](https://cranlogs.r-pkg.org/badges/restatapi)](https://mybinder.org/v2/gh/mmatyi/restatapi_logs/b1320a7cd483638e1f12c8a1f5bf595cbbc32233?urlpath=shiny/ShinyApps/restatapi/)
[![all downloads](https://cranlogs.r-pkg.org/badges/grand-total/restatapi)](https://mmatyi.github.io/restatapi_logs/)
<!-- badges: end -->

# restatapi
An R package to search and retrieve data from Eurostat database using SDMX  

# <span style="color:red">IMPORTANT changes with the new Eurostat API</span>

Version 0.20.0 enables all the functionality for the [new dissemination chain](https://wikis.ec.europa.eu/display/EUROSTATHELP/Developer%27s+corner) and from version 0.20.3 it is the default API.

The new API has **breaking changes** concerning the `date_filter`. In the old dissemination chain the value was assigned to *the first day* of the month, quarter and year, so it was enough to filter for one day to get the value for the whole period. Under the new API the value belongs to the full period. If a date range does not cover the whole period no value is returned. For example, to get the value of the whole quarter the date filter should start at least on the first date of the quarter and end at least on the last day of the quarter. With exact numerical example to get the value for 2022/Q3, the `startDate` should be 2022-07-01 or earlier and the `endDate` should be 2022-09-30 or later. In the old version of the API it was enough if the period included the day 2022-07-01 only. 

In addition to this change, if the date filter is only one day (e.g. `startDate=2007-07-02&endDate=2007-07-02`) then the new API gives back the values for all the time periods in the dataset applying the filter provided for the other concepts. But if the time period changes to more than one day (e.g. `startDate=2007-07-01&endDate=2007-07-02`) then the new API gives back only those values which are covered by the range. For more details see the updated description of the numerical examples in [Example 6](#updated-date-filter). 


## installation

'restatapi' can be installed from [CRAN](https://CRAN.R-project.org/package=restatapi) by 

```R
install.packages("restatapi")
```

or use the development version from GitHub

```R
remotes::install_github("eurostat/restatapi")
```

## background
This package is similar to other packages like the [eurodata](https://github.com/alekrutkowski/eurodata), [eurostat](https://cran.r-project.org/package=eurostat), [rdbnomics](https://cran.r-project.org/package=rdbnomics), [RJSDMX](https://cran.r-project.org/package=RJSDMX) or [TSsdmx](https://cran.r-project.org/package=TSsdmx) which can be used to download data from Eurostat database. The difference is that `restatapi` is based on SDMX (Statistical Data and Metadata eXchange) and XML to search and retrieve filtered datasets and use the TSV (tab separated values) bulk download facility to get whole data tables. The code was written in a way that the number of dependencies on other packages should be very small. The `restatapi` package provides flexible filtering options, data caching, and uses the `parallel` and `data.table` package to handle large dataset in an efficient way.  

## content
The package contains 8 main functions and several other sub functions in 3 areas.

1. downloading and filtering the list of available datasets:
    * the `get_eurostat_toc` function downloads the Table of Contents (TOC) of all [Eurostat datasets](https://ec.europa.eu/eurostat/web/main/data/database).
    * the `search_eurostat_toc` function provides the facility to search for phrase, pattern and regular expressions in the TOC and returns the rows of the TOC where the search string(s) found.
2. downloading and searching in the metadata:  
    * the `get_eurostat_dsd` function returns the Data Structure Definition (DSD) of a given dataset containing the possible dimensions and values with their labels. 
    * the `search_eurostat_dsd` function provides the facility to search for phrase, pattern and regular expressions in the DSD and returns the rows of the DSD where the search string(s) found.
    * the `create_filter_table` function creates a filter table based on the DSD and the search expressions which can be applied on the local computer to filter out data from whole data tables.
3. retrieving full or partial datasets:
    * the `get_eurostat_raw` function downloads the full data table as it is either using the TSV format (default) or the SDMX format keeping all the column names and rows as it is in the original files.
    * the `get_eurostat_bulk` function downloads the full data set keeping only a unique frequency with standardized column names and removing those columns which do not contain additional information, like frequency and time format.  
    * the `get_eurostat_data` function retrieves a data table which can be labeled using the labels from the DSD. The table can contain the whole datasets or only part of it if filters are applied. If after the filtering the number of observations is to large, then the whole dataset is downloaded and the filter applied on the local computer. If no filter used, it is equivalent with the `get_eurostat_bulk` function, but in this case labels can be applied.

Below there are examples demonstrating the main features, the detailed documentation of the functions is in the package.

Next to the functions the package contains a list of country codes for different groups of European countries based on the [Eurostat standard code list](https://ec.europa.eu/eurostat/ramon/nomenclatures/index.cfm?TargetUrl=LST_NOM_DTL&StrNom=CL_GEO&StrLanguageCode=EN&IntPcKey=48517911&StrLayoutCode=HIERARCHIC), e.g.: European Union (EU28, ..., EU6), Euro Area (EA19, ..., EA11) or New Member States (NMS13, ..., NMS2).

## 10 examples

**Example 1:** First set the number of cores/threads (`restatapi_cores`) to 3 and download the XML version of the the English Table of Contents (TOC). Then change the file download method (`dmethod`) from the default `auto` to `libcurl` in case there is problem with the system defaults, and download the txt version (`mode="txt"`) of the TOC showing the detailed debugging messages (`verbose=TRUE`). The debugging information can show if there is a problem with the internet connection, as it provides the URL which is used to download the data from the API. The provided URL can be copied and checked in a regular browser if the API gives a response or not. Finally, search not case sensitive (`ignore.case=TRUE`) for the word `energie` in the German version (`lang="de"`) of the TOC. 

```R
options(restatapi_cores=3)
get_eurostat_toc()
options(restatapi_dmethod="libcurl")
get_eurostat_toc(mode="txt",verbose=TRUE)
search_eurostat_toc("energie",lang="de",ignore.case=TRUE)
```

**Example 2:** Download the Data Structure Definition (DSD) for the 'Financial services - quarterly data' (`ei_bsfs_q`) datasets. First search in the DSD the "EU" pattern everywhere case sensitive. Then search only in the code list (`name=FALSE`).  

```R
dsd<-get_eurostat_dsd("ei_bsfs_q")
search_eurostat_dsd("EU",dsd)
search_eurostat_dsd("EU",dsd,name=FALSE)
```


**Example 3:** Download the raw dataset `avia_gor_me` ('Freight and mail air transport between the main airports of Montenegro and their main partner airports (routes data)') from the Tab Separated Value (tsv) file and keep it as it is (`melt=FALSE`). In his case each time period will be in separate columns. Then download the same whole dataset using the SDMX (`xml`) version. In this case the data will be melted. Finally using the `get_eurostat_bulk` function to download the same dataset.  In this case the data will contain only a unique time period (the most frequent one - monthly data) with standardized column names ('time', 'values' and 'flags' in case `keep_flags=TRUE` used).  

```R
get_eurostat_raw("avia_gor_me",melt=FALSE)
get_eurostat_raw(" Avia_gor_ME","xml")
get_eurostat_bulk("AVIA_GOR_ME ")
```

**Example 4:** Download the whole data table of 'GDP and main components' (`nama_10_gdp`), then check if the provided `id` is in the Table of Contents (`check_toc=TRUE`). If it is correct, then do not use the cached version from the memory (`.restatapi_env`), but rather download it again and update the dataset in the cache (`update_cache=TRUE`). Finally, change the cache directory from memory to hard disk to a temporary folder (`cache_dir="/tmp"`, `/tmp` is a typical temporary folder on Unix-like systems) and download there the whole data table keeping all non-numeric values as string instead of converting to factors (`stringAsFactors=FALSE`), and keeping the lines without values which were suppressed due to confidentiality (having the 'c' flag, `cflags=TRUE`). 

```R
get_eurostat_data("NAMA_10_GDP")
get_eurostat_data("nama_10_gdp",update_cache=TRUE,check_toc=TRUE)
get_eurostat_data("nama_10_gdp",cache_dir="/tmp",stringAsFactors=FALSE,cflags=TRUE)
```

**Example 5:** Set the option, that always download the dataset from the internet and do not use the cached version of the data(`restatapi_update=TRUE`). In this case it is not necessary define this option for each data query. Then change the default cache from  memory (`.restatapi_env`) to hard disk (`restatapi_cache_dir=file.path(tempdir(),"restatapi")`), to the folder `restatapi` inside the R temporary folder. This is the default cache directory on the hard disk, which will be created, in case the provided `cache_dir` folder does not exist.

```R
options(restatapi_update=TRUE)
options(restatapi_cache_dir=file.path(tempdir(),"restatapi"))
```
<a name="updated-date-filter"></a>
**Example 6:** First download the annual (`select_freq="A"`) air passenger transport data for the main airports of Montenegro (`avia_par_me`) and do not cache any of the data (`cache=FALSE`). Then from the same table download the monthly (`select_freq="M"`) and quarterly (`filters="Q...`) data for 2 specific airport pairs/routes (`filters=...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK"`) in August 2016 and on 1 July 2017 (`date_filter=c("2016-08","2017-07-01")`). The filters are provided in the format how it is required by the [REST SDMX web service](https://wikis.ec.europa.eu/pages/viewpage.action?pageId=44165555). Under the old API, it returned the value for the selected routes for the month August 2016, July 2017 and the 3rd quarter of 2017. Meanwhile under the ***new API***, it returns all the quarterly and monthly value, as there is a single day in the `date_filter`.
Then download again the monthly and quarterly data (`filters=c("Quarterly","Monthly")`) where there is exact match in the DSD for "HU" for August 2016 and 1 March 2014 (`date_filter=c("2016-08","2014-03-01")`). This query will provide only monthly data for 2016, as the quarterly data is always assigned to the first month of the quarter and there is no data for 2014. Since there is no exact match for the "HU" pattern, it returned all the monthly data for August 2016 and put the labels (like the name of the airports and units) so the data can be easier understood (`label=TRUE`) under the old API. Under the ***new API***, it returns all the quarterly and monthly data as there is a single day in the `date_filter`.
Finally, download only the quarterly data (`select_freq="Q"`) for several time periods (`date_filter=c("2017-03",2016,"2017-07-01",2012:2014)`, the order of the dates does not matter) where the "HU" pattern can be found anywhere, but only in the `code` column of the DSD (`filters="HU",exact_match=FALSE,name=FALSE`). The result was all the statistics about flights from Montenegro to Hungary in the 3rd quarter of 2017, as there is no information for the other time periods under the old API. Under the ***new API***, it gives back all the quarterly data in dataset for flights from Montenegro to Hungary because in the `date_filter` there is a single day. 

```R
dt<-get_eurostat_data("avia_par_me",select_freq="A",cache=FALSE)
dt<-get_eurostat_data("avia_par_me",
                       filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",
                       date_filter=c("2016-08","2017-07-01"),
                       select_freq="M") 
dt<-get_eurostat_data("avia_par_me",
                       filters=c("HU","Quarterly","Monthly"),
                       date_filter=c("2016-08","2014-03-01"),
                       label=TRUE)
dt<-get_eurostat_data("avia_par_me",
                       filters="HU",
                       exact_match=FALSE,
                       date_filter=c("2017-03",2016,"2017-07-01",2012:2014),
                       select_freq="Q",
                       label=TRUE,
                       verbose=TRUE,
                       name=FALSE)
```

**Example 7:** Download from the Time Use Survey (TUS) data (`tus_00age`) for 2010 (`date_filter=2010`) in Hungary how much time spent with travel on average. If someone does not know the exact codes then the filter patterns (`filters=c("total","time spent","HU",'travel')`) can be searched in the labels (`label=TRUE`) non case sensitive (`ignore.case=TRUE`) without forcing exact match of the patterns (`exact_match=FALSE`). The first function call will result an empty data table as the SDMX webservice will provide NaN (Not a Number) response for the time values (hh:mm). But these values are included in the bulk download files, and can be retrieved by forcing the filtering on the local computer (`force_local_filter=TRUE`). Then the retrieved values can be summed using the [chron](https://cran.r-project.org/package=chron) package.

```R
dt<-get_eurostat_data("tus_00age",
                       filters=c("HU","total","time spent",'travel'),
                       date_filter=2010,
                       exact_match=FALSE,
                       ignore.case=TRUE,
                       label=TRUE)
dt<-get_eurostat_data("tus_00age",
                       filters=c("HU","total","time spent",'travel'),
                       date_filter=2010,
                       force_local_filter=TRUE,
                       exact_match=FALSE,
                       ignore.case=TRUE,
                       label=TRUE)
dt
dt[acl00!="Total",sum(chron::times(paste0(values,":00")))]                       
```

**Example 8:** Download the data on the production of cow's milk on farms by NUTS 2 regions (`agr_r_milkpr`) first only for the new Member States joined in 2004 and keeping only the period between March 2009 and 5 June 2011 (`date_filter="2009-03:2011-06-05"`). The country code of the member states can be loaded from the `.restatapi env` (`eu<-get("cc",envir=.restatapi_env)`) and it can be used in the query (`filters=eu$NMS10`). Then get all the data for Belgium for all [NUTS](https://ec.europa.eu/eurostat/en/web/products-manuals-and-guidelines/-/ks-gq-20-092) level from the same data set before July 2009 (`date_filter="<2009-07"`) with the labels (`label=TRUE`) and the observation status (so called *flags*) information (`flags=TRUE`). In this case for filter (`filters="BE"`) the exact matching of pattern should be turned off (`exact_match=FALSE`) to get not just at country (NUTS0) level. Finally, get the data at for Hungary at NUTS2 level after 19 May 2017 (`date_filter="2017-05-19<`) and keeping the lines which were removed due to confidentiality (`cflags=TRUE`). For this we do not have to know the exact code, name or number of the NUTS2 regions as we can provide regular expression in the filter (`filters=c("^HU..")`) and providing the option that the expression is Perl compatible (`perl=TRUE`).    

```R
eu<-get("cc",envir=.restatapi_env)
dt<-get_eurostat_data("agr_r_milkpr",
                       filters=eu$NMS10,
                       date_filter="2009-03:2011-06-05")
dt<-get_eurostat_data("agr_r_milkpr",
                      filters="BE",
                      date_filter="<2009-07",
                      keep_flags=TRUE,
                      exact_match=FALSE,
                      label=TRUE)
dt<-get_eurostat_data("agr_r_milkpr",
                       filters=c("^HU.."),
                       date_filter="2017-05-19<",
                       cflags=TRUE,
                       perl=TRUE)                       
```


**Example 9:** Download the balance (`stk_flow="BAL"`) from the international trade in services dataset (`bop_its6_det`) for 2020 (`date_filter=2020`), transport services (`bop_item="SC"`), with reporting countries Hungary and the EU (`geo=c("EU27_2020","HU")`) and trading partner outside EU (`partner="EXT_EU27_2020"`). In order to avoid that Hungary shows up in the partner countries the filter should be defined as a named list (`filters=list(bop_item="SC",partner="EXT_EU27_2020",geo=c("EU27_2020","HU"),stk_flow="BAL")`) and do not search for the terms in the labels (`name=FALSE`). In this case the filter patterns only searched where the concept equals to the name.  

```R
dt<-get_eurostat_data("bop_its6_det",
                       filters=list(bop_item="SC",
                                    partner="EXT_EU27_2020",
                                    geo=c("EU27_2020","HU"),
                                    stk_flow="BAL"),
                        date_filter=2020,
                        label=TRUE,
                        name=FALSE)     
```

**Example 10:** After finishing the tasks the cache (in memory and on the hard drive) can be cleaned up. 

```R
clean_restatapi_cache(tempdir(),verbose=TRUE)
```


