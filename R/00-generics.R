add_steprun <- function(x, steprun, idx, ...) {
  UseMethod("add_steprun")
}

run <- function(object, state, ...) {
  UseMethod("run")
}