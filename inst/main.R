if (interactive() && requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
} else {
  library(PEITHO)
}

PEITHO::startApplication(launch.browser = TRUE)
