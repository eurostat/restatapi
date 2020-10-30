# restatapi
An R package to search and retrieve data from Eurostat database using SDMX  

## installation

Now 'restatapi' is on [CRAN](https://CRAN.R-project.org/package=restatapi) so it can be installed by 

```R
> install.packages("restatapi")
```

or use the development version from GitHub

```R
> devtools::install_github("eurostat/restatapi")
```

## background
This package is similar to other packages like the [eurostat](https://cran.r-project.org/package=eurostat), [rdbnomics](https://cran.r-project.org/package=rdbnomics), [RJSDMX](https://cran.r-project.org/package=RJSDMX) or [TSsdmx](https://cran.r-project.org/package=TSsdmx) which can be used to download data from Eurostat database. The difference is that `restatapi` is based on SDMX (Statistical Data and Metadata eXchange) and XML to search and retrieve filtered datasets and use the TSV (tab separated values) bulk download facility to get whole data tables. The code was written in a way that the number of dependencies on other packages should be very small. The `restatapi` package provides flexible filtering options, data caching, and uses the `parallel` and `data.table` package to handle large dataset in an efficient way.  

## content
The package contains 8 main functions and several other sub functions in 3 areas.

1. downloading and filtering the list of available datasets:
    * the `get_eurostat_toc` function downloads the Table of Contents (TOC) of all [Eurostat datasets](https://ec.europa.eu/eurostat/data/database).
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

Next to the functions the package contains a list of country codes for different groups of European countries based on the [Eurostat standard code list](https://ec.europa.eu/eurostat/ramon/nomenclatures/index.cfm?TargetUrl=LST_NOM_DTL&StrNom=CL_GEO&StrLanguageCode=EN&IntPcKey=42277583&IntResult=1&StrLayoutCode=HIERARCHIC), e.g.: European Union (EU28, ..., EU6), Euro Area (EA19, ..., EA11) or New Member States (NMS13, ..., NMS2).

## examples
**Example 1:** Setting the number of cores/threads to 3 and download the txt version of the English TOC and search not case sensitive for the word `energie` in the German version of the TOC. 

```R
> options(restatapi_cores=3)
> get_eurostat_toc()
> get_get_eurostat_toc(mode="txt",verbose=TRUE)
> search_eurostat_toc("energie",lang="de",ignore.case=TRUE)
```
**Example 2:** Download the Data Structure Definition (DSD) for the 'Financial services - quarterly data' (ei_bsfs_q) datasets. First search in the DSD the "EU" pattern everywhere case sensitive. Then search only in the code list.  
```R
> dsd<-get_eurostat_dsd("ei_bsfs_q")
> search_eurostat_dsd("EU",dsd)
> search_eurostat_dsd("EU",dsd,name=FALSE)
```
**Example 3:** Download the whole data table of 'GDP and main components', then check if the provided `id` is in the Table of Contents. If it is correct, then do not use the cached version from the memory (`.restatapi_env`), but rather download it again and update the dataset in the cache. Finally, change the cache directory from memory to hard disk, to a temporary folder (`/tmp` in the Unix-like systems) and download there the whole data table keeping all non-numeric values as string instead of factor, and keeping the lines without values which were suppressed due to confidentiality (having the 'c' flag). 
```R
> get_eurostat_data("NAMA_10_GDP")
> get_eurostat_data("nama_10_gdp",update_cache=TRUE,check_toc=TRUE)
> get_eurostat_data("nama_10_gdp",cache_dir="/tmp",stringAsFactors=FALSE,cflags=TRUE)
```
**Example 4:** Set the option, that always download the dataset from the internet and do not use the cached version of the data. In this case it is not necessary define for each data query. Then change the default cache from  memory (`.restatapi_env`) to hard disk, to the folder `restatapi` inside the R temporaty folder. This is the default cache directory on the hard disk which will be created, in case the provided `cache_dir` folder does not exist.
```R
> options(restatapi_update=TRUE)
> options(restatapi_cache_dir=file.path(tempdir(),"restatapi"))
```
**Example 5:** First download the annual (`select_freq="A"`) air passenger transport data for the main airports of Montenegro and do not cache any of the data. Then from the same table download the monhly (`select_freq="M"`) and quarterly (`filters="Q...`) data for 2 specific airport pairs/routes (`filters=...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK"`) in August 2016 and on 1 July 2017. Then download the again the monthly and quarterly data (`filters=c("Quarterly","Monthly")`) where there is the exact match in the DSD for "HU" for August 2016 and 1 March 2014. This query will provide only monthly data for 2016 as the quarterly data is always assigned to the first month of the quarter and there is no data for 2014. As there is no exact match for the "HU" pattern, it will return all the monthly data for August 2016 and put the labels (like the name of the airports and units) so the data can be easier undertood. Finally, download only the quarterly data (`select_freq="Q"`) for several time periods (`date_filter=c("2017-03",2016,"2017-07-01",2012:2014)`) where the "HU" pattern can be found anywhere, but only in the `code` column of the DSD (`filters="HU",exact_match=FALSE,name=FALSE`). The result will be all the statistics about flights from Montenegro to Hungary in the 3rd quarter of 2017, as there is no information for the other time periods.
```R
> dt<-get_eurostat_data("avia_par_me",select_freq="A",cache=FALSE)
> dt<-get_eurostat_data("avia_par_me",
>                       filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",
>                       date_filter=c("2016-08","2017-07-01"),
>                       select_freq="M") 
> dt<-get_eurostat_data("avia_par_me",
>                       filters=c("HU","Quarterly","Monthly"),
>                       date_filter=c("2016-08","2014-03-01"),
>                       label=TRUE)
> dt<-get_eurostat_data("avia_par_me",
>                        filters="HU",
>                        exact_match=FALSE,
>                        date_filter=c("2017-03",2016,"2017-07-01",2012:2014),
>                        select_freq="Q",
>                        label=TRUE,
>                        verbose=TRUE,
>                        name=FALSE)
```

**Other examples:**
```R
> dt<-get_eurostat_data("agr_r_milkpr",date_filter=2008,keep_flags=TRUE)
> dt<-get_eurostat_data("bop_its6_det",
>                        filters=list(bop_item="SC",
>                                     currency="MIO_EUR",
>                                     partner="EXT_EU28",
>                                     geo=c("EU28","HU"),
>                                     stk_flow="BAL"),
>                        date_filter="2010:2012",
>                        select_freq="A",
>                        label=TRUE,
>                        name=FALSE,
>                        ignore.case=TRUE)     
> dt<-get_eurostat_data("agr_r_milkpr",
>                       filters=c("BE$","Hungary"),
>                       date_filter="2007-06<",
>                       keep_flags=TRUE)
> dt<-get_eurostat_data("agr_r_milkpr",
>                       filters="BE",
>                       exact_match=FALSE,
>                       date_filter="2006-02:2008-06-05",
>                       keep_flags=TRUE,
>                       stringsAsFactors=FALSE,
>                       label=TRUE,
>                       ignore.case=TRUE)
>
> options(restatapi_cache_dir=tempdir())
> eu<-get("cc",envir=.restatapi_env)
> dt<-get_eurostat_data("agr_r_milkpr",
>                       filters=eu$NMS10,
>                       date_filter="2009-03-01:2011-06-05",
>                       keep_flags=TRUE,
>                       stringsAsFactors=FALSE,
>                       label=TRUE)
>
> dt<-get_eurostat_data("nama_10_a10_e",
>                       filters=c("Annual","EU28","Belgium","AT","Total","EMP_DC","person"),
>                       date_filter=c("2008",2002,"2005-01",2013:2018))
>
> clean_restatapi_cache(tempdir(),verbose=TRUE)

```
