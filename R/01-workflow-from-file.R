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

get_commands <- function(path_to_folder, command_file = "commands.json") {
  read_json_if_exists(file.path(path_to_folder, command_file))
}

get_inputs <- function(
  path_to_folder,
  #input_file  = "inputs.json",
  input_file  = "inputs.txt",
  pattern = "@#\\*I\\*#@"
) {
  input_path <- file.path(path_to_folder, input_file)

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

get_results <- function(path_to_folder, result_file = "results_summary.json") {
  read_json_if_exists(file.path(path_to_folder, result_file))
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
  path_to_folder
) {
  if (is_input_tag(arg)) {
    input_list <- get_inputs(path_to_folder = path_to_folder)

    varname <- extract_tag_varname(arg, "^@#\\*I\\*#@(.*)@#\\*I\\*#@$")

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
      label    = varname,
      type     = "input",
      loop     = cmd_loop %||% "no"
    )
  } else if (is_result_tag(arg)) {
    result_list <- get_results(path_to_folder = path_to_folder)

    varname <- extract_tag_varname(arg, "^@#\\*L\\*#@(.*)@#\\*L\\*#@$")

    if (!any(varname %in% sapply(result_list, function(res) res$name))) {
      stop(
        "Variable '", varname, "' not found in results file.",
        call. = FALSE
      )
    }
    new_operationparam(
      step_id = step_i,
      position = arg_i,
      name     = arg_name,
      value    = NULL,
      label    = varname,
      type     = "result",
      loop     = cmd_loop %||% "no"
    )
  } else {
    # literal without name (no tag)
    new_operationparam(
      step_id = step_i,
      position = arg_i,
      name     = arg_name,
      value    = strip_outer_quotes(arg),
      label    = "",
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

#' Extract workflow steps from files in a folder
#'
#' @param path_to_folder Path to folder containing `commands.json` and `inputs.txt` and `results_summary.json`.
#'  Default is the package's `peitho_files` folder.
#' @return A list of `workflowstep` objects.
#' @export
extract_workflow_from_files <- function(
  path_to_folder = system.file("scripts", "peitho_files", package = "PEITHO")
) {
  # if folder not found return empty list and warn
  if (!dir.exists(path_to_folder)) {
    warning("PEITHO files not found. No folder '", path_to_folder, "'. Returning empty workflow.")
    return(list())
  }

  # check if all files exist
  if (!file.exists(file.path(path_to_folder, "commands.json"))) {
    warning("commands.json not found in folder '", path_to_folder, "'. Returning empty workflow.")
    return(list())
  }
  if (!file.exists(file.path(path_to_folder, "inputs.json")) && !file.exists(file.path(path_to_folder, "inputs.txt"))) {
    warning("inputs.txt not found in folder '", path_to_folder, "'. Returning empty workflow.")
    return(list())
  }

  commands_list <- get_commands(path_to_folder = path_to_folder)

  steps <- lapply(seq_along(commands_list), function(step_i) {
    cmd <- commands_list[[step_i]]

    parsed   <- parse_args(cmd$args)
    args_vec <- parsed$values
    args_names <- parsed$names

    # create params
    params <- vector("list", length(args_vec))
    for (arg_i in seq_along(args_vec)) {
      arg_name <- args_names[arg_i]
      if (!nzchar(arg_name)) arg_name <- NULL

      params[[arg_i]] <- make_param_from_arg(
        arg      = args_vec[[arg_i]],
        arg_name = arg_name,
        arg_i    = arg_i,
        step_i   = step_i,
        cmd_loop = cmd$loop,
        path_to_folder = path_to_folder
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
      loop            = cmd$loop %||% "no"
    )
  })

  steps
}
