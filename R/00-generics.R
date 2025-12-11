update <- function(x, steprun, idx, ...) {
  UseMethod("update")
}

run <- function(object, state, ...) {
  UseMethod("run")
}