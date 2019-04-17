# restatapi
An R package to search and retrieve data from Eurostat database using SDMX  

## installation

```R
> devtools::install_github("eurostat/restatapi")
```

## background
This package is similar to other packages like the [eurostat](https://cran.r-project.org/package=eurostat), [rdbnomics](https://cran.r-project.org/package=rdbnomics) or [TSsdmx](https://cran.r-project.org/package=TSsdmx) which can be used to download data from Eurostat database. The difference is that `restatapi` is based on SDMX (Statistical Data and Metadata eXchange) and XML to search and retrieve datasets and was developed with small number of dependencies on other packages. The `restatapi` package provides flexible filtering options, data caching, and uses the `parallel` and `data.table` package to handle large dataset in an efficient way.  

## content
The package contains 5 main functions and several other sub functions: 

* the `get_eurostat_toc` function downloads the Table of Contents (TOC) of all [Eurostat datasets](https://ec.europa.eu/eurostat/data/database),
* the `search_eurostat_toc` function provides the facility to search for phrase/pattern in the TOC and returns the rows of the TOC where the phrase/pattern found.
* the `get_eurostat_dsd` function returns the Data Structure Definition (DSD) of a given dataset containing the possible dimensions and values with their labels. 
* the `search_eurostat_dsd` function provides the facility to search for phrase/pattern in the DSD and returns the rows of the DSD where the phrase/pattern found.
* the `get_eurostat_data` function retrieves a data table which can be labeled using the labels from the DSD. The table can contain the whole datasets or only part of it if filters are applied.

Detailed documentation of the functions is in the package.


## examples

```R
> get_eurostat_toc()
> get_get_eurostat_toc(mode="txt",verbose=TRUE)
>
> search_eurostat_toc("energie",lang="de",ignore.case=T)
> 
> dsd<-get_eurostat_dsd("ei_bsfs_q")
> search_eurostat_dsd("EU",dsd)
> 
> get_eurostat_data("NAMA_10_GDP")
> get_eurostat_data("nama_10_gdp",update_cache=TRUE)
> get_eurostat_data("nama_10_gdp",cache_dir="/tmp",stringAsFactors=FALSE)
>
> options(restatapi_update=TRUE)
> options(restatapi_cache_dir=file.path(tempdir(),"restatapi"))
>
> dt<-get_eurostat_data("avia_par_me",select_freq="A",cache=FALSE)
> dt<-get_eurostat_data("agr_r_milkpr",date_filter=2008,keep_flags=TRUE)
> dt<-get_eurostat_data("avia_par_ee",
>                        filters="BE$",
>                        date_filter=c("2017-03",2016,"2017-07-01",2012:2014),
>                        select_freq="Q",
>                        label=TRUE,
>                        verbose=TRUE)
> options(restatapi_cache_dir=tempdir())
> dt<-get_eurostat_data("agr_r_milkpr",
>                       filters=c("BE$","Hungary"),
>                       date_filter="2007-06<",
>                       keep_flags=TRUE)
> dt<-get_eurostat_data("agr_r_milkpr",
>                       filters="BE",
>                       date_filter="2006-02:2008-06-05",
>                       ignore.case=TRUE,
>                       keep_flags=TRUE,
>                       stringsAsFactors=FALSE,
>                       label=TRUE)
> dt<-get_eurostat_data("nama_10_a10_e",
>                       filters=c("Annual","EU28","Belgium","AT","Total","EMP_DC","person"),
>                       date_filter=c("2008",2002,"2005-01",2013:2018))
> dt<-get_eurostat_data("avia_par_me",
>                       filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",
>                       date_filter=c("2016-08","2017-07-01"),
>                       select_freq="M") 
> dt<-get_eurostat_data("avia_par_me",
>                       filters=c("HU","Quarterly","Monthly"),
>                       date_filter=c("2016-08","2014-03-01"),
>                       label=TRUE)
>
> clean_restatapi_cache(tempdir(),verbose=TRUE)

```
