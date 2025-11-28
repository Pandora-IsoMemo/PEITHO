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

run_step <- function(step, state, ...) {
  UseMethod("run_step")
}

add_steprun <- function(state, steprun, ...) {
  UseMethod("add_steprun")
}

run_workflow <- function(wf, state = NULL, ...) {
  UseMethod("run_workflow")
}