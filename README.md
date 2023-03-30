
<!-- README.md is generated from README.Rmd. Please edit that file -->

# downloader

<!-- badges: start -->
<!-- badges: end -->

An improved `download.file` function that:

- retries to download the file multiple times with or without a
  **delay**.
- checks if the file to download already exists in the path and decide
  if overwrite it.
- provides a full log with info regarding warning and errors of each
  download.

## Installation

You can install the development version of downloader with:

``` r
devtools::install_github("c1au6i0/downloader")
```

## Example

This is a basic example

``` r
library(downloader)

# 2 csv files to download
to_download <- c(
  "https://www.stats.govt.nz/assets/Uploads/Business-employment-data/Business-employment-data-September-2022-quarter/Download-data/Business-employment-data-september-2022-quarter-csv.zip", 
  "www.stats.govt.nz/assets/Uploads/Business-financial-data/Business-financial-data-September-2022-quarter/Download-data/business-financial-data-september-2022-quarter-csv.zip"
  )
logs_download <- retry_download(conn  = to_download, 
               max_attempts = 5, 
               sleep_time = 1, 
               dest_path = tempdir(),  
               )
#> 
#> ── Download  attempt 1 ─────────────────────────────────────────────────────────
#> ℹ success
#> 
#> ── Download  attempt 1 ─────────────────────────────────────────────────────────
#> ℹ success
#> 
```

Here the logs generated:

``` r
logs_download
#>                                                                                                                                                                                  url_file
#> 1 https://www.stats.govt.nz/assets/Uploads/Business-employment-data/Business-employment-data-September-2022-quarter/Download-data/Business-employment-data-september-2022-quarter-csv.zip
#> 2            www.stats.govt.nz/assets/Uploads/Business-financial-data/Business-financial-data-September-2022-quarter/Download-data/business-financial-data-september-2022-quarter-csv.zip
#>                                                      file     out details retry
#> 1 Business-employment-data-september-2022-quarter-csv.zip success success     1
#> 2  business-financial-data-september-2022-quarter-csv.zip success success     1
```
