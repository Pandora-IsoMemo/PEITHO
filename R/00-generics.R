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

add_steprun <- function(x, steprun, idx, ...) {
  UseMethod("add_steprun")
}

extract_arg <- function(x, last_result, ...) {
  UseMethod("extract_arg")
}

run_workflow <- function(wf, state = NULL, ...) {
  UseMethod("run_workflow")
}