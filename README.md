# restatapi
An R package to search and retrieve data from Eurostat database using SDMX  

## installation

```R
> devtools::install_github("eurostat/restatapi")
```

## background
This package is based on the [eurostat](https://github.com/rOpenGov/eurostat/) package. The difference is that `restatapi` is based on SDMX (Statistical Data and Metadata eXchange) and XML to search and retrieve datasets. This package provides flexible filtering options and data caching, and uses `parallel` and `data.table` package to handle large dataset in an efficient way.  

## content
The package contains 5 main functions and several other sub functions: 

* the `get_eurostat_toc` function downloads the Table of Contents (TOC) of all [Eurostat datasets](https://ec.europa.eu/eurostat/data/database),
* the `search_eurostat_toc` function provides the facility to search for phrase/pattern in the TOC and returns the rows of the TOC where the phrase/pattern is found.
* the `get_eurostat_dsd` function returns the Data Structure Definition (DSD) of a given dataset containing the possible dimensions and values with their labels. 
* the `search_eurostat_dsd` function provides the facility to search for phrase/pattern in the DSD and returns the rows of the DSD where the phrase/pattern is found.
* the `get_eurostat_data` function retrieves a data table which can be labeled using the labels from the DSD. The table can contain the whole datasets or only part of it if filters are applied.

Detailed documentation of the functions is in the package.


## examples

```R
> get_eurostat_toc()
> search_eurostat_toc("energie",lang="de",ignore.case=T)
> 
> dsd<-get_eurostat_dsd("ei_bsfs_q")
> search_eurostat_dsd("EU",dsd)
> 
> get_eurostat_data("NAMA_10_GDP")
> get_eurostat_data("nama_10_gdp",update_cache=TRUE)
> get_eurostat_data("nama_10_gdp",cache_dir="/tmp",stringAsFactors=F)
> options(restatapi_update=TRUE)
> options(restatapi_cache_dir=file.path(tempdir(),"restatapi"))
>
> dt<-get_eurostat_data("avia_par_me",select_time="A",cache=F)
> 
> dt<-get_eurostat_data("agr_r_milkpr",date_filter=2020,keep_flags=T)
```
