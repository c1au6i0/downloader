#' download hyperlink
#'
#' Use download.file to download from hyperlink
#'
#' @param conn Url.
#' @param dest_path Folder where to save files.
#' @param overwrite If TRUE overwrite the file.
#' @param ... Any arguments of `download.file`.
#' @export
download_file <- function(conn, dest_path, overwrite = FALSE, ...) {
  path_out <- file.path(dest_path, basename(conn))

  if (fs::file_exists(path_out)) {
    if (overwrite) {
      fs::file_delete(path_out)
      cli::cli_alert_warning("Overwriting {.file {path_out}}.")
    } else {
      return(list(url_file = conn, file = basename(conn), out = "failed", details = "file already present"))
    }
  }

  tryCatch(
    {
      utils::download.file(url = conn, destfile = path_out, quite = FALSE, ...)
      return(list(url_file = conn, file = basename(conn), out = "success", details = "success"))
    },
    error = function(cond) {
      cond <- gsub("download.file(url = conn, destfile = path_out, quite = FALSE, ...)", "", cond)
      return(list(url_file = conn, file = basename(conn), out = "failed", details = cond))
    },
    warning = function(cond) {
      cond <- gsub("download.file(url = conn, destfile = path_out, quite = FALSE, ...)", "", cond)
      return(list(url_file = conn, file = basename(conn), out = "warn", details = cond))
    }
  )
}

#' retry download internal
#'
#' Use download_files, try to download from links and save stout or err in dataframe.
#'
#' @param conn Url.
#' @param dest_path Folder where to save files.
#' @param max_attempts How many times to try to download.
#' @param sleep_time How many second to wait before retring.
#' @param ... Any `download.file` argument.
retry_download_ <- function(conn, dest_path, max_attempts, sleep_time, ...) {
  # Modified from this
  # https://stackoverflow.com/questions/63340463/download-files-until-it-works
  attempts <- 0
  file_name <- basename(conn)
  ret_eval <- list(url_file = conn, file = file_name, out = 0, details = NA)

  while (ret_eval$out %in% c("failed", "warn", 0)) {

    if (sleep_time > 0) Sys.sleep(sleep_time)

    if (attempts >= max_attempts) {
      cli::cli_par()
      cli::cli_alert_danger("File {.file {file_name}} not downloaded.")
      return(c(ret_eval, retry = attempts))
    } else {
      cli::cli_par()
      cli::cli_h1("Download  attempt{?s} {cli::no(attempts +1)}")
      ret_eval <- download_file(conn, dest_path, ...)
      cli::cli_alert_info(ret_eval$details)
      cli::cli_end()
      # Stop if file already present
      if (ret_eval$details == "file already present") {
        attempts <- max_attempts
      } else {
        attempts <- attempts + 1
      }
    }
  }

  c(ret_eval, retry = attempts)
}


#' retry download
#'
#' Use download_files, try to download from links and save stout or err in dataframe.
#'
#' @param conn Vectors of connections (urls).
#' @param dest_path Folder where to save files.
#' @param max_attempts How many times to try to download.
#' @param sleep_time How many second to wait before retring.
#' @param ... Any arguments of `download.file`.
#' @export
retry_download <- function(conn, dest_path, max_attempts, sleep_time, ...) {

  if(length(conn) == 1) {

    out <- retry_download_(conn = conn,
                          dest_path = dest_path,
                          max_attempts = max_attempts,
                          sleep_time = sleep_time,
                          ...)
  }

  if(length(conn) > 1) {

      out <- lapply(conn,
                                   retry_download_,
                                   dest_path = dest_path,
                                   max_attempts = max_attempts,
                                   sleep_time = sleep_time,
                                   ...
                                   )
      out <-  do.call(rbind, out)
    }

  out
}




