# Helper functions for workflow extraction from files

normalize_varname <- function(x) {
  x <- trimws(x)
  gsub(" ", "_", x)
}

extract_tag_varname <- function(x, pattern) {
  varname <- sub(pattern, "\\1", x)
  normalize_varname(varname)
}

read_json_if_exists <- function(path) {
  if (!file.exists(path)) return(list())
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

get_inputs <- function(
  path_to_folder,
  inputs_file,
  pattern = "@#\\*I\\*#@"
) {
  input_path <- file.path(path_to_folder, inputs_file)

  # load input.json or input.txt
  if (grepl("\\.json$", input_path)) {
    input_list <- read_json_if_exists(input_path)
  } else if (grepl("\\.txt$", input_path)) {
    if (file.exists(input_path)) {
      lines <- readLines(input_path)
      tag_list_indx <- grepl(paste0("^", pattern, ".*", pattern, "$"), lines)
      tag_list <- unique(lines[tag_list_indx])
      # for all tags find lines in between and store as list
      input_list <- list()
      for (tag in tag_list) {
        start_indx <- which(lines == tag)[1] + 1
        end_indx <- which(lines == tag)[2] - 1
        if (is.na(end_indx)) {
          end_indx <- length(lines)
        }

        varname <- extract_tag_varname(tag, paste0("^", pattern, "(.*)", pattern, "$"))
        input_list[[varname]] <- paste(lines[start_indx:end_indx], collapse = "\n")
      }
    } else {
      input_list <- list()
    }
  } else {
    stop(
      "Unsupported input file format: ", input_path,
      call. = FALSE
    )
  }

  input_list
}

is_input_tag <- function(x) {
  grepl("^@#\\*I\\*#@.*@#\\*I\\*#@$", x)
}

is_result_tag <- function(x) {
  grepl("^@#\\*L\\*#@.*@#\\*L\\*#@$", x)
}

make_param_from_arg <- function(
  arg,
  arg_name,
  step_i,
  arg_i,
  cmd_loop,
  path_to_folder,
  inputs_file
) {
  if (is_input_tag(arg)) {
    varname <- extract_tag_varname(arg, "^@#\\*I\\*#@(.*)@#\\*I\\*#@$")
    input_list <- get_inputs(path_to_folder = path_to_folder, inputs_file = inputs_file)

    if (!varname %in% names(input_list)) {
      stop(
        "Input variable '", varname, "' not found in input file.",
        call. = FALSE
      )
    }
    new_operationparam(
      step_id = step_i,
      position = arg_i,
      name     = arg_name,
      value    = input_list[[varname]],
      type     = "input",
      tag      = "@#\\*I\\*#@",
      label    = varname,
      loop     = cmd_loop %||% "no"
    )
  } else if (is_result_tag(arg)) {
    varname <- extract_tag_varname(arg, "^@#\\*L\\*#@(.*)@#\\*L\\*#@$")

    new_operationparam(
      step_id = step_i,
      position = arg_i,
      name     = arg_name,
      value    = NULL,
      type     = "result",
      tag      = "@#\\*L\\*#@",
      label    = varname,
      loop     = cmd_loop %||% "no"
    )
  } else {
    # literal without name (no tag)
    new_operationparam(
      step_id = step_i,
      position = arg_i,
      name     = arg_name,
      value    = strip_outer_quotes(arg),
      type     = "literal",
      loop     = cmd_loop %||% "no"
    )
  }
}

parse_args <- function(arg_string) {
  if (is.null(arg_string) || !nzchar(arg_string)) {
    return(list(values = character(0), names = character(0)))
  }
  # split on commas, but not inside single or double quotes
  args_vec <- strsplit(
    arg_string,
    ",(?=(?:[^']*'[^']*')*[^']*$)(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)",
    perl = TRUE
  )[[1]] |> trimws()

  arg_names <- character(length(args_vec))
  arg_values <- character(length(args_vec))
  for (i in seq_along(args_vec)) {
    x <- args_vec[[i]]
    if (grepl("=", x)) {
      x_split <- trimws(strsplit(x, "=", fixed = TRUE)[[1]])
      arg_names[i] <- x_split[[1]]
      arg_values[i]  <- x_split[[2]]
    } else {
      # positional / unnamed argument
      arg_names[i] <- ""
      arg_values[i]  <- x
    }
  }

  list(values = arg_values, names = arg_names)
}

strip_outer_quotes <- function(x) {
  x <- trimws(x)
  if (nchar(x) < 2L) return(x)

  first <- substr(x, 1L, 1L)
  last  <- substr(x, nchar(x), nchar(x))

  if (first == last && first %in% c("'", "\"")) {
    substr(x, 2L, nchar(x) - 1L)
  } else {
    x
  }
}

load_workflow_script_env <- function(script_path, parent_env, show_functions_path = TRUE) {
  if (is.null(script_path)) return(parent_env)
  if (!file.exists(script_path)) {
    stop("Custom script file not found: ", script_path, call. = FALSE)
  }
  if (is_running_online()) {
    PEITHO:::logWarn("Running online; skipping loading custom script.")
    return(parent_env)
  }
  if (show_functions_path) {
    PEITHO:::logInfo("Loading custom script for workflow: %s", script_path)
  }
  script_env <- new.env(parent = parent_env)
  sys.source(script_path, envir = script_env)
  return(script_env)
}

#' Extract workflow steps from files in a folder
#'
#' @param workflow_file_paths A list of file paths for workflow files (see `workflow_file_paths()`).
#'  Default is the package's `peitho_files` folder.
#' @param show_functions_path Logical, whether to show the path of the loaded script file.
#' @return A list of `workflowstep` objects.
#' @export
extract_workflow_from_files <- function(workflow_file_paths, show_functions_path = TRUE) {
  # if folder not found return empty list and warn
  if (!dir.exists(workflow_file_paths$path_to_folder)) {
    PEITHO:::logWarn(
      "PEITHO files not found. No folder '%s'. Returning empty workflow.",
      workflow_file_paths$path_to_folder
    )
    return(list())
  }

  # check if all files exist
  if (!file.exists(workflow_file_paths$inputs_path)) {
    PEITHO:::logWarn(
      "%s not found in folder '%s'. Returning empty workflow.",
      basename(workflow_file_paths$inputs_path),
      workflow_file_paths$path_to_folder
    )
    return(list())
  }
  if (!file.exists(workflow_file_paths$commands_path)) {
    PEITHO:::logWarn(
      "%s not found in folder '%s'. Returning empty workflow.",
      basename(workflow_file_paths$commands_path),
      workflow_file_paths$path_to_folder
    )
    return(list())
  }
  if (!file.exists(workflow_file_paths$results_path)) {
    PEITHO:::logInfo("Creating empty %s file.", basename(workflow_file_paths$results_path))
    jsonlite::write_json(
      list(),
      workflow_file_paths$results_path,
      auto_unbox = TRUE,
      pretty = TRUE
    )
  }
  if (!file.exists(workflow_file_paths$functions_path)) {
    PEITHO:::logInfo(
      "%s not found in folder '%s'. Using global environment for operations.",
      basename(workflow_file_paths$functions_path),
      workflow_file_paths$path_to_folder
    )
    env <- parent.frame()
  } else {
    env <- load_workflow_script_env(
      script_path = workflow_file_paths$functions_path,
      parent_env = parent.frame(),
      show_functions_path = show_functions_path
    )
  }

  PEITHO:::logDebug("Loading commands from %s", workflow_file_paths$commands_path)
  commands_list <- read_json_if_exists(path = workflow_file_paths$commands_path)

  PEITHO:::logDebug("Extracting %d workflow steps from commands", length(commands_list))
  steps <- lapply(seq_along(commands_list), function(step_i) {
    cmd <- commands_list[[step_i]]

    PEITHO:::logInfo("Parsing command %s for step %d", cmd$command, step_i)
    parsed   <- parse_args(cmd$args)
    args_vec <- parsed$values
    args_names <- parsed$names

    # create params
    params <- vector("list", length(args_vec))
    for (arg_i in seq_along(args_vec)) {
      PEITHO:::logDebug("  Processing argument %d: %s", arg_i, args_vec[[arg_i]])
      arg_name <- args_names[arg_i]
      if (!nzchar(arg_name)) arg_name <- NULL

      params[[arg_i]] <- make_param_from_arg(
        arg      = args_vec[[arg_i]],
        arg_name = arg_name,
        arg_i    = arg_i,
        step_i   = step_i,
        cmd_loop = cmd$loop,
        path_to_folder = workflow_file_paths$path_to_folder,
        inputs_file = basename(workflow_file_paths$inputs_path)
      )
    }

    # create workflowstep
    new_workflowstep(
      id              = step_i,
      name            = cmd$name %||% paste0("Step ", step_i),
      label           = cmd$label %||% cmd$name %||% paste0("Step ", step_i),
      comments        = cmd$comments %||% "",
      operation       = cmd$command,
      params          = params %||% list(),
      loop            = cmd$loop %||% "no",
      env             = env
    )
  })

  steps
}
