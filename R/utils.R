#' extract_hypelinks
#'
#' Use XML to extract hyperlinks and generate a list
#'
#' @param path_file Path to xlsx file with hyperlinks
#' @returns A vector of hyperlinks.
#' @export
extract_hyperlinks <- function(path_file) {

  # https://stackoverflow.com/questions/24149821/extract-hyperlink-from-excel-file-in-r
  zip_file <- sub("xlsx", "zip", path_file)

  file.copy(from = path_file, to = zip_file)
  path_out <- tempdir()
  utils::unzip(zip_file, exdir = path_out)
  xml <- XML::xmlParse(fs::path(path_out, "xl", "worksheets", "sheet1.xml"))
  hyperlinks <- XML::xpathApply(xml, "//x:hyperlink/@display", namespaces = "x")

  unlink(path_out, recursive = TRUE)
  unlink(zip_file)
  as.character(hyperlinks)
}
