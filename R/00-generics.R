current_step <- function(x, ...) {
  UseMethod("current_step")
}

next_step <- function(x, ...) {
  UseMethod("next_step")
}

previous_step <- function(x, ...) {
  UseMethod("previous_step")
}

goto_step <- function(x, ...) {
  UseMethod("goto_step")
}

add_steprun <- function(x, steprun, idx, ...) {
  UseMethod("add_steprun")
}

run <- function(object, state, ...) {
  UseMethod("run")
}