# restatapi 0.12.4

- correcting timeout of examples for the check with -run-donttest

# restatapi 0.12.3

- adding timeout to examples
- not testing example for the `search_eurostat_dsd()` and `create_filter_table()` function

# restatapi 0.12.2

- not testing example for the `extract_dsd()` and `create_filter_table()` function
- removing link to the SDMX website because of SSL error

# restatapi 0.12.1

- replacing example in the `extract_dsd()` fuction to reduce time for testing
- checking the footer for code >=500 when the data table has 0 row

# restatapi 0.12.0

- discarding partial data if the retrieval stopped due to "Internal application error", "Exception while getting all data and footnotes slice" or "Cannot connect to Comext service"
- seprating blocks with conditions in testing

# restatapi 0.11.3

- using full URI path in README.md
- CRAN release

# restatapi 0.11.2

- unzipping to a temporary directory in order to avoid error by read-only user directory

# restatapi 0.11.1

- correction of tests when the TOC is corrupted
- correction of documentation

# restatapi 0.10.10

- correction of the `verbose` option in the `create_filter_table()` function

# restatapi 0.10.9

- checking if download file is empty in the `get_eurostat_toc()` function when `mode=txt`
- correction of tests

# restatapi 0.10.8

- correction of tables in the documentation


# restatapi 0.10.7

- further correction to clean up all cache files from the hard disk
- checking if DSD is `NULL` in the `create_filter_table()` function when `date_filter=FALSE`
- `filter_raw_data()` returns the raw data when `filter_table=NULL`
- correction of the `create_filter_table()` call in the `get_eurostat_data()` function
- using `tryCatch()` for opening the tsv files in the `get_eurostat_toc()` function
- correction of tests when the data/DSD download unsuccessful 
- CRAN release

# restatapi 0.10.6

- correction to clean up all cache files from the hard disk
- CRAN release

# restatapi 0.10.5

- correction for the `get_compressed_sdmx()` function when `verbose=FALSE`

# restatapi 0.10.4

- improved debugging messages
- `date_filter` can be a variable name
- additional test

# restatapi 0.10.3

- links with https
- CRAN release

# restatapi 0.10.2

- `.restatpi_env` disconnected from `baseenv()`

# restatapi 0.10.1

- correction of labeling when values are non-numeric

# restatapi 0.10.0

- testing with `tinytest`
- improved debugging messages
- correction of `get_eurostat_bulk()` when `cflags=TRUE`
- setting download method by `options(restatapi_dmethod=...)`
- `libcurl` is default method only in Windows 
- CRAN release

# restatapi 0.9.12

- updated examples

# restatapi 0.9.11

- corrected when `force_local_filter` used in the `get_eurostat_data()` function
- hiding download progress bar in the `get_eurostat_raw()` function when `verbose=FALSE`
- corrected testing

# restatapi 0.9.10

- added caching of raw files when `local_filter` applied

# restatapi 0.9.9

- correction of `date_filter` for local filtering when quarterly data filtered

# restatapi 0.9.8

- updated documentation 
- additional tests
- CRAN release

# restatapi 0.9.7

- adding option for non-melted data output in the `get_eurostat_raw()` function 
- update of tests

# restatapi 0.9.5

- updated revised documentation

# restatapi 0.9.0

- correction for the disabled years above 2100 in the `date_filter`

# restatapi 0.8.10

- correction for negative values in the `get_eurostat_raw()` function when `mode=txt`
- updated documentation
- correction for local filtering with additional tests
- CRAN release

# restatapi 0.8.7

- correction if `select_freq` defined when `filters` are named list 
- additional tests

# restatapi 0.8.6

- correction for local filtering
- refactoring the `get_eurostat_raw()` function
- CRAN release

# restatapi 0.8.3

- correction for local filtering
- revision of testing 

# restatapi 0.8.1

- added logging of parameters for the `get_eurostat_data()` function

# restatapi 0.8.0

- added  `create_filter_table()` function to be used for filtering on local computer
- additional testing and documentation
- CRAN release

# restatapi 0.7.11

- added function `filter_raw_data()` for filtering on local computer

# restatapi 0.7.7

- correction to correctly handle non-numerical (Time Use Survey) data

# restatapi 0.7.5

- `date_filter` creates the date filter table
- use of `tryCatch` for parallel processing in Windows
- CRAN release

# restatapi 0.7.3

- removed memory size check for MacOS and Solaris
- CRAN release

# restatapi 0.7.1

- correction for checking the footer message of the XML response
- additional tests
- CRAN release

# restatapi 0.6.9

- updated documentation
- bug fixes
- CRAN release

# restatapi 0.6.7

- BREXIT update of the config file

# restatapi 0.6.6

- checking the footer message of the XML response when filter used in the  `get_eurostat_data()` function

# restatapi 0.6.4

- filtering by named list

# restatapi 0.6.2

- correction of memory size check under Solaris
- CRAN release

# restatapi 0.6.0

- correction of the `extract_data` function under Windows
- updated documentation
- CRAN release

# restatapi 0.5.6

- added `cflags` option for the `get_eurostat_data()` and `get_eurostat_bulk()` function

# restatapi 0.5.1

- removed dependency on `rsdmx`

# restatapi 0.4.14

- checking the memory size in the `load_cfg()` function to determine the number of cores for `parallel`

# restatapi 0.4.12

- additional tests
- bug fixes

# restatapi 0.4.10

- added `check_toc` option for the `get_eurostat_bulk()` function 

# restatapi 0.4.7

- added `check_toc` option for the `get_eurostat_raw()` function 

# restatapi 0.4.5

- checking the version of `data.table` package in the `get_eurostat_raw()` function 

# restatapi 0.4.3

- `extract_data` function can handle filtered and bulk XML files 

# restatapi 0.4.1

- improved debugging messages
- add option to download and cache the TOC with the config file

# restatapi 0.4.0

- additional tests
- bug fixes
- CRAN release

# restatapi 0.3.12

- bug fixes, additional info for debugging for the `get_eurostat_dsd()` function

# restatapi 0.3.10

- "libcurl" is the default file download method if available 

# restatapi 0.3.9

- separated file download and data extraction for the `get_eurostat_dsd()` function

# restatapi 0.3.8

- bug fixes, additional info for debugging

# restatapi 0.3.7

- checking for `NA` in the TOC download links

# restatapi 0.3.6

- adding the `exact_match` option for filtering in the `search_eurostat_dsd()` function
- CRAN release

# restatapi 0.3.5

- bug fixes
- CRAN release

# restatapi 0.3.2

- adding the `get_compressed_sdmx()` function

# restatapi 0.3.1

- correction for `select_freq` when `name=FALSE` in the `get_eurostat_data()` function 

# restatapi 0.3.0

- working parallelisation under Windows for all the functions

# restatapi 0.2.6

- working parallelisation under Windows for the `get_eurostat_toc()` function

# restatapi 0.2.4

- additional test and examples

# restatapi 0.2.3

- separated file download and data extraction for the `txt` option in the `get_eurostat_raw()` function
- CRAN release

# restatapi 0.2.2

- adding the `txt` option to the `get_eurostat_raw()` function

# restatapi 0.1.10

- corrected documentation

# restatapi 0.1.9

- adding the `exact_match` option for filtering in the `get_eurostat_data()` function
- CRAN release

# restatapi 0.1.8

- inclusion of the EU country groups in the config file

# restatapi 0.1.7

- correction for `strinsAsFactors` option after filtering
- added documentation with `pkgdown`

# restatapi 0.1.6

- revision for the CRAN release

# restatapi 0.1.4

- first CRAN submission
- use of `tryCatch()` to capture errors caused by network unavailability
- messages instead of warnings
- loading config file from GitHub

# restatapi 0.1.3

- changing examples

# restatapi 0.1.2

- additional tests and correction in caching

# restatapi 0.1.0

- first release on GitHub
