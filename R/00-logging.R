logInfo <- function(msg, ...) {
  futile.logger::flog.info(msg, ...)
}

logDebug <- function(msg, ...) {
  futile.logger::flog.debug(msg, ...)
}

logWarn <- function(msg, ...) {
  futile.logger::flog.warn(msg, ...)
}

init_logging <- function() {
  if (as.logical(Sys.getenv("SHOW_DEBUG", unset = "FALSE"))) {
    futile.logger::flog.threshold(futile.logger::DEBUG)
    packageStartupMessage("Logger initialized: threshold = DEBUG")
  } else {
    futile.logger::flog.threshold(futile.logger::INFO)
    packageStartupMessage("Logger initialized: threshold = INFO")
  }
}