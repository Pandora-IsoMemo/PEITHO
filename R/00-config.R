#' Load PEITHO configuration
#'
#' Loads the PEITHO configuration from the `config.yaml` file in the package.
#' @return A list containing the configuration values.
#' @export
config <- function() {
  config_path <- system.file("config.yaml", package = "PEITHO")
  yaml::yaml.load_file(config_path)
}
