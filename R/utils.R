#' extract_hypelinks
#'
#' Use XML to extract connlinks and generate a list
#'
#' @param path_file Path to xlsx file with hyperlinks.
#' @export
extract_hyperlinks <- function(path_file) {
  # https://stackoverflow.com/questions/24149821/extract-hyperlink-from-excel-file-in-r
  zip_file <- sub("xlsx", "zip", path_file)

  file.copy(from = path_file, to = zip_file)
  utils::unzip(zip_file, exdir = here::here("data", "xl_parse"))
  xml <- XML::xmlParse(here::here("data", "xl_parse", "xl", "worksheets", "sheet1.xml"))
  hyperlinks <- XML::xpathApply(xml, "//x:hyperlink/@display", namespaces = "x")

  unlink(here::here("data", "xl_parse"), recursive = TRUE)
  unlink(zip_file)
  as.character(hyperlinks)
}

#' download hyperlink
#'
#' Use download.file to download from hyperlink
#'
#' @param conn Url.
#' @param dest_path Folder where to save files.
#' @param overwrite
#' @param ... Any arguments of `download.file`.
#' @export
download_file <- function(conn, dest_path, overwrite = FALSE, ...) {
  path_out <- file.path(dest_path, basename(conn))

  if (fs::file_exists(dest_path)) {
    if (overwrite) fs::file_delete(dest_out)
    if (!overwrite) {
      return(list(url_file = conn, file = basename(conn), out = "failed", details = "file altrady present"))
    }
  }

  tryCatch(
    {
      download.file(url = conn, destfile = path_out, quite = FALSE, ...)
      return(list(url_file = conn, file = basename(conn), out = "success", details = NA))
    },
    error = function(cond) {
      return(list(url_file = conn, file = basename(conn), out = "failed", details = cond))
    },
    warning = function(cond) {
      return(list(url_file = conn, file = basename(conn), out = "warn", details = cond))
    }
  )
}

#' retry download
#'
#' Use download_files, try to download from links and save stout or err in dataframe.
#'
#' @param connUrl.
#' @param dest_path Folder where to save files.
#' @param max_attempts How many times to try to download.
#' @param sleep_time How many second to wait between retry.
#' @export
retry_download <- function(conn, dest_path, max_attempts, sleep_time, ...) {
  # Modified from this
  # https://stackoverflow.com/questions/63340463/download-files-until-it-works
  attempts <- 0
  ret_eval <- list(url_file = conn, file = basename(conn), out = 0, details = NA)

  while (ret_eval$out %in% c("failed", "warn", 0)) {
    if (sleep_time > 0) Sys.sleep(sleep_time)

    if (attempts >= max_attempts) {
      return(c(ret_eval, retry = attempts))
    } else {
      message(paste0("\n\n Download n: ", attempts + 1, " \n"))
      ret_eval <- download_file(conn, dest_path, ...)
      message(ret_eval$details)
      attempts <- attempts + 1
    }
  }

  c(ret_eval, retry = attempts)
}



#' #' retry downbload and create folder
#' #'
#' #' Use download_files, try to download from links and save stout or err in dataframe.
#' #' It also create a folder in dest_path with run name
#' #'
#' #' @param conn Dataframe with column `run, Sample, FASTQ`. run values need to be all the same.
#' #' @param dest_path Folder where to save files.
#' #' @param max_attempts How many times to try to download.
#' #' @param sleep_time How many second to wait between retry.
#' retry_download_df <- function(conn, dest_path, max_attempts, sleep_time, ...) {
#'
#'   # Modified from this
#'   # https://stackoverflow.com/questions/63340463/download-files-until-it-works
#'
#'   conn<- conn[[1]]
#'   if (length(unique(conn$run)) != 1) stop("Multiple values in run")
#'
#'
#'   name_fq_folder <- unique(conn$run)
#'
#'   dest_path_full <- file.path(dest_path, name_fq_folder)
#'
#'   if (!fs::dir_exists(dest_path_full)) {
#'     fs::dir_create(dest_path_full)
#'   }
#'
#'   conn_vec <- as.character(conn$FASTQ)
#'
#'   # dat <- lapply(conn_vec, retry_download, dest_path = dest_path_full, max_attempts, sleep_time, ...)
#'   # dplyr::bind_rows(dat)
#'
#'   out <- c()
#'
#'   for (hyp in conn_vec) {
#'     out_int <- retry_download(conn = hyp, dest_path = dest_path_full, max_attempts = max_attempts, sleep_time = sleep_time, ...)
#'     out <- dplyr::bind_rows(out, out_int)
#'     out
#'   }
#'
#'   out
#' }
