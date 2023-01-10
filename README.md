
<!-- README.md is generated from README.Rmd. Please edit that file -->

# downloader

<!-- badges: start -->
<!-- badges: end -->

An improved `download.file` function that:

- retries to download the file multiple times with or without a delay.
- checks if the file to download already exists in the path and decide
  if overwrite it.
- can parallel download.
- provides a full log with info regarding warning and errors of each
  download.

## Installation

You can install the development version of downloader like so:

``` r
devtools::install_github("c1au6i0/downloader")
```

## Example

This is a basic example

``` r
library(downloader)
to_download # list of connections to data to download
retry_download(conn  = to_download, 
               max_attempts = 5, 
               sleep_time = 1, 
               dest_path = tempdir(),  
               workers = 10)
```
