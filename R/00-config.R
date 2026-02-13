config <- function() {
  config_path <- system.file("config.yaml", package = "PEITHO")
  yaml::yaml.load_file(config_path)
}
