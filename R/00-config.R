#' Load PEITHO configuration
#'
#' Loads the PEITHO configuration from the `config.yaml` file in the package.
#' @return A list containing the configuration values.
#' @export
config <- function() {
  config_path <- system.file("config.yaml", package = "PEITHO")
  if (!file_nonempty(config_path)) {
    stop("Config file not found at: ", config_path, call. = FALSE)
  }
  yaml::yaml.load_file(config_path)
}
