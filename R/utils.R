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

  fs::dir_delete(here::here("data", "xl_parse"))
  fs::file_delete(zip_file)
  as.character(hyperlinks)
}
