#' @rawNamespace import(shiny)

utils::globalVariables(
  c()
)

#' @importFrom DataTools build_download_zip import_bundle_zip importServer importUI
#' @importFrom futile.logger DEBUG flog.debug flog.info flog.threshold flog.warn INFO
#' @importFrom httr2 request req_perform req_timeout req_user_agent resp_status resp_body_html
#' @importFrom jsonlite fromJSON toJSON write_json
#' @importFrom rvest html_element html_elements html_text2
#' @importFrom stats setNames
#' @importFrom stringr str_squish
#' @importFrom tibble tibble
#' @importFrom utils str
#' @importFrom yaml yaml.load_file
NULL
