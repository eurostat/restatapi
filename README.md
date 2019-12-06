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
This package is similar to other packages like the [eurostat](https://cran.r-project.org/package=eurostat), [rdbnomics](https://cran.r-project.org/package=rdbnomics), [RJSDMX](https://cran.r-project.org/package=RJSDMX) or [TSsdmx](https://cran.r-project.org/package=TSsdmx) which can be used to download data from Eurostat database. The difference is that `restatapi` is based on SDMX (Statistical Data and Metadata eXchange) and XML to search and retrieve filtered datasets and use the TSV (tab separeted values) bulk download facility to get whole data tables. The code was written in a way that the number of dependencies on other packages should be very small. The `restatapi` package provides flexible filtering options, data caching, and uses the `parallel` and `data.table` package to handle large dataset in an efficient way.  

## content
The package contains 5 main functions and several other sub functions: 

* the `get_eurostat_toc` function downloads the Table of Contents (TOC) of all [Eurostat datasets](https://ec.europa.eu/eurostat/data/database),
* the `search_eurostat_toc` function provides the facility to search for phrase/pattern in the TOC and returns the rows of the TOC where the phrase/pattern found.
* the `get_eurostat_dsd` function returns the Data Structure Definition (DSD) of a given dataset containing the possible dimensions and values with their labels. 
* the `search_eurostat_dsd` function provides the facility to search for phrase/pattern in the DSD and returns the rows of the DSD where the phrase/pattern found.
* the `get_eurostat_data` function retrieves a data table which can be labeled using the labels from the DSD. The table can contain the whole datasets or only part of it if filters are applied.

Detailed documentation of the functions is in the package.

Next to the functions the package contains a list of country codes for different groups of European countries based on the [Eurostat standard code list](https://ec.europa.eu/eurostat/ramon/nomenclatures/index.cfm?TargetUrl=LST_NOM_DTL&StrNom=CL_GEO&StrLanguageCode=EN&IntPcKey=42277583&IntResult=1&StrLayoutCode=HIERARCHIC), e.g.: European Union (EU28, ..., EU6), Euro Area (EA19, ..., EA11) or New Member States (NMS13, ..., NMS2).

## examples

```R
> options(restatapi_cores=3)
> get_eurostat_toc()
> get_get_eurostat_toc(mode="txt",verbose=TRUE)
>
> search_eurostat_toc("energie",lang="de",ignore.case=TRUE)
> 
> dsd<-get_eurostat_dsd("ei_bsfs_q")
> search_eurostat_dsd("EU",dsd)
> search_eurostat_dsd("EU",dsd,name=FALSE)
> 
> get_eurostat_data("NAMA_10_GDP")
> get_eurostat_data("nama_10_gdp",update_cache=TRUE,check_toc=TRUE)
> get_eurostat_data("nama_10_gdp",cache_dir="/tmp",stringAsFactors=FALSE,cflags=TRUE)
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
>                        verbose=TRUE,
>                        name=FALSE)
> options(restatapi_cache_dir=tempdir())
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
