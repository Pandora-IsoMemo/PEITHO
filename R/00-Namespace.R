#' @rawNamespace import(shiny)

utils::globalVariables(
  c()
)

#' @importFrom DataTools build_download_zip import_bundle_zip importServer importUI
#' @importFrom DT datatable DTOutput formatStyle renderDT
#' @importFrom futile.logger DEBUG flog.debug flog.info flog.threshold flog.warn INFO
#' @importFrom httr2 request req_perform req_retry req_timeout req_user_agent resp_status
#'  resp_body_html
#' @importFrom jsonlite fromJSON toJSON write_json
#' @importFrom llmModule ask_llm
#' @importFrom rvest html_element html_elements html_text2
#' @importFrom shinyTools headerButtonsUI shinyTryCatch
#' @importFrom shinyTree renderTree shinyTree
#' @importFrom stats setNames
#' @importFrom stringr str_remove_all str_replace_all str_squish
#' @importFrom tibble tibble
#' @importFrom utils capture.output str write.csv
#' @importFrom xml2 xml_remove
#' @importFrom yaml yaml.load_file
NULL
