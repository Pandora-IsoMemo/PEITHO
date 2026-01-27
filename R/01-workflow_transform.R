# R/workflow_transform.R
# ------------------------------------------------------------------------------
# Workflow transformation + reference resolution helpers
#
# This file intentionally groups helpers that:
#  - transform workflows/steps (rename, renumber, combine, rewrite references)
#  - resolve "result" references at runtime (step name vs step id)
#
# Rationale:
# Workflows are often linear when "result" params are interpreted as state$last_result.
# As soon as we want joins/branching (e.g., combine last outputs of two sub-workflows),
# we must be able to fetch *specific* prior results by reference. We also need consistent
# renaming/rewriting when combining workflows to keep step names unique and references valid.
# Keeping this logic in one place makes future changes (name-based vs id-based refs) safe.

# ---- runtime: resolve stored results by name/id ------------------------------

# NOTE: Result reference resolver (future-proofing)
# -----------------------------------------------
# Even if current workflows are mostly linear via state$last_result, we keep this resolver
# so that result-params can reference a specific prior step (by name and/or id).
# This is required for joins / branching and for combined workflows where names/ids change.
#
# Expected state caches (if present):
#   state$results_by_id   : list with keys "id_<int>"
#   state$results_by_name : list with keys "name_<string>"
#
# If these caches are not present, this function errors with an informative message.
resolve_result_from_state <- function(state, ref, prefer = c("name", "id")) {
  prefer <- match.arg(prefer)
  stopifnot(inherits(state, "workflowstate"))

  if (is.null(state$results_by_id) || is.null(state$results_by_name)) {
    stop(
      "State does not contain result caches (results_by_id/results_by_name). ",
      "Either extend workflowstate to store results, or implement a stepruns-based resolver.",
      call. = FALSE
    )
  }

  # ref may be:
  # - numeric/integer step id
  # - character step name
  # - character digits (interpretable as id)
  if (is.numeric(ref) && length(ref) == 1L && !is.na(ref)) {
    key <- paste0("id_", as.integer(ref))
    val <- state$results_by_id[[key]]
    if (is.null(val)) stop("Result for step id ", ref, " not found.", call. = FALSE)
    return(val)
  }

  if (is.character(ref) && length(ref) == 1L && nzchar(ref)) {
    if (prefer == "name") {
      key <- paste0("name_", ref)
      val <- state$results_by_name[[key]]
      if (!is.null(val)) return(val)

      if (grepl("^[0-9]+$", ref)) {
        key2 <- paste0("id_", as.integer(ref))
        val2 <- state$results_by_id[[key2]]
        if (!is.null(val2)) return(val2)
      }
    } else {
      if (grepl("^[0-9]+$", ref)) {
        key <- paste0("id_", as.integer(ref))
        val <- state$results_by_id[[key]]
        if (!is.null(val)) return(val)
      }
      key2 <- paste0("name_", ref)
      val2 <- state$results_by_name[[key2]]
      if (!is.null(val2)) return(val2)
    }

    stop("Result reference '", ref, "' not found.", call. = FALSE)
  }

  stop("Invalid result reference type.", call. = FALSE)
}

# ---- transformation: rename/rewrite/renumber steps ---------------------------

# Build mapping old_name -> new_name using a prefix/suffix scheme.
# Use this when combining sub-workflows to ensure unique step names.
make_step_name_map <- function(steps, prefix = NULL, suffix = NULL) {
  stopifnot(is.list(steps))
  old <- vapply(steps, `[[`, character(1), "name")

  if (!is.null(prefix) && nzchar(prefix)) old2 <- paste0(prefix, "__", old) else old2 <- old
  if (!is.null(suffix) && nzchar(suffix)) old2 <- paste0(old2, "__", suffix)

  stats::setNames(old2, old)
}

# Rewrite result references stored in operationparam$label using a name map.
# This is the object-level equivalent of rewriting @#*L*#@...@#*L*#@ tags in commands.json.
rewrite_result_param_labels <- function(step, name_map) {
  if (length(step$params) == 0L) return(step)

  step$params <- lapply(step$params, function(p) {
    if (inherits(p, "operationparam") && identical(p$type, "result")) {
      if (!is.null(p$label) && nzchar(p$label) && !is.null(name_map[[p$label]])) {
        p$label <- name_map[[p$label]]
      }
    }
    p
  })
  step
}

# Rename workflowstep$name (and optionally label) and rewrite all result-param labels.
# Returns list(steps=..., name_map=...).
rename_steps_and_rewrite_refs <- function(steps, prefix = NULL, suffix = NULL, rename_label = FALSE) {
  name_map <- make_step_name_map(steps, prefix = prefix, suffix = suffix)

  out <- lapply(steps, function(s) {
    old_name <- s$name
    s$name <- name_map[[old_name]]
    if (isTRUE(rename_label) && !is.null(s$label) && identical(s$label, old_name)) {
      s$label <- s$name
    }
    rewrite_result_param_labels(s, name_map)
  })

  list(steps = out, name_map = name_map)
}

# Renumber step ids sequentially (and keep operationparam$step_id consistent).
renumber_steps <- function(steps, start = 1L) {
  n <- length(steps)
  if (n == 0L) return(steps)

  new_ids <- seq.int(as.integer(start), length.out = n)

  out <- lapply(seq_along(steps), function(i) {
    s <- steps[[i]]
    s$id <- as.integer(new_ids[i])

    if (length(s$params)) {
      s$params <- lapply(s$params, function(p) {
        if (inherits(p, "operationparam")) p$step_id <- s$id
        p
      })
    }
    s
  })

  out
}

# Validate:
#  1) unique step names
#  2) all operationparam(type="result") labels refer to an existing step name
validate_step_graph <- function(steps) {
  if (length(steps) == 0L) return(invisible(TRUE))

  step_names <- vapply(steps, `[[`, character(1), "name")
  if (anyDuplicated(step_names)) {
    dup <- unique(step_names[duplicated(step_names)])
    stop("Duplicate step names detected: ", paste(dup, collapse = ", "), call. = FALSE)
  }

  refs <- unlist(lapply(steps, function(s) {
    if (!length(s$params)) return(character(0))
    vapply(s$params, function(p) {
      if (inherits(p, "operationparam") && identical(p$type, "result")) p$label else NA_character_
    }, character(1))
  }), use.names = FALSE)

  refs <- refs[!is.na(refs) & nzchar(refs)]
  missing <- setdiff(unique(refs), step_names)
  if (length(missing)) {
    stop(
      "Broken workflow references: these referenced step names do not exist: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# ---- high-level: combine workflows + add join step ---------------------------

# Combine two workflows into a new workflow with unique step names and valid references.
# - prefixes help avoid collisions
# - steps are renumbered
combine_workflows <- function(wf1, wf2, prefix = c("WF1", "WF2"), rename_label = FALSE) {
  stopifnot(inherits(wf1, "workflow"), inherits(wf2, "workflow"))
  stopifnot(is.character(prefix), length(prefix) == 2L)

  a <- rename_steps_and_rewrite_refs(wf1$steps, prefix = prefix[1], rename_label = rename_label)
  b <- rename_steps_and_rewrite_refs(wf2$steps, prefix = prefix[2], rename_label = rename_label)

  steps <- c(a$steps, b$steps)
  steps <- renumber_steps(steps, start = 1L)

  validate_step_graph(steps)

  new_workflow(
    name = paste0(wf1$name, " + ", wf2$name),
    steps = steps,
    use_peitho_folder = FALSE,
    current = if (length(steps)) 1L else NA_integer_
  )
}

# Create a join step that combines the *last outputs* of two workflows (or two step names).
# The join operation must accept args x and y (plus optional sep/...)
make_join_last_step <- function(
  name = "JOIN__combine_last",
  label = "Combine last outputs",
  operation = "combine_last_outputs",
  ref_x,
  ref_y,
  sep = "\n---\n",
  env = parent.frame()
) {
  # ref_x/ref_y are step references stored in operationparam$label:
  # typically step names; can also be ids-as-character if you later choose.

  new_workflowstep(
    id = NA_integer_,  # will be renumbered after insertion
    name = name,
    label = label,
    operation = operation,
    params = list(
      new_operationparam(step_id = NA_integer_, position = 1, name = "x", type = "result", label = ref_x, loop = "no"),
      new_operationparam(step_id = NA_integer_, position = 2, name = "y", type = "result", label = ref_y, loop = "no"),
      new_operationparam(step_id = NA_integer_, position = 3, name = "sep", type = "literal", value = sep, loop = "no")
    ),
    loop = "no",
    env = env
  )
}

# Append a join-last step to a workflow and renumber.
append_step_and_renumber <- function(wf, step) {
  stopifnot(inherits(wf, "workflow"), inherits(step, "workflowstep"))
  steps <- c(wf$steps, list(step))
  steps <- renumber_steps(steps, start = 1L)
  validate_step_graph(steps)

  wf$steps <- steps
  wf$current <- if (length(steps)) 1L else NA_integer_
  wf$workflow_file_paths <- list()
  wf
}

# ---- export: workflow -> commands.json-like list -----------------------------

# Convert one operationparam back into a commands.json argument string fragment.
# This mirrors your original tags:
#  - input  -> @#*I*#@label@#*I*#@
#  - result -> @#*L*#@label@#*L*#@
#  - literal -> value (quoted if needed)
param_to_arg_string <- function(p) {
  stopifnot(inherits(p, "operationparam"))

  val <- switch(
    p$type,
    input  = paste0("@#*I*#@", p$label, "@#*I*#@"),
    result = paste0("@#*L*#@", p$label, "@#*L*#@"),
    literal = {
      v <- p$value
      if (is.null(v)) "NULL" else {
        v <- as.character(v)
        # quote if it contains comma or spaces or backslashes
        if (grepl("[ ,\\\\\"]", v)) paste0("\"", gsub("\"", "\\\\\"", v), "\"") else v
      }
    },
    stop("Unknown param type: ", p$type, call. = FALSE)
  )

  if (!is.null(p$name) && nzchar(p$name)) paste0(p$name, "=", val) else val
}

# Build a commands.json-like structure from a workflow object.
workflow_to_commands_list <- function(wf) {
  stopifnot(inherits(wf, "workflow"))

  lapply(wf$steps, function(s) {
    args <- if (!length(s$params)) "" else {
      paste(vapply(s$params, param_to_arg_string, character(1)), collapse = ", ")
    }

    list(
      entry    = s$id,
      name     = s$name,
      label    = s$label %||% s$name,
      comments = s$comments %||% "",
      command  = s$operation,
      args     = args,
      loop     = s$loop %||% "no",
      prompt   = ""  # fill if you later store prompts in workflowstep$dots or elsewhere
    )
  })
}

# Write commands.json for a workflow.
export_commands_json <- function(wf, path, pretty = TRUE) {
  jsonlite::write_json(
    workflow_to_commands_list(wf),
    path,
    auto_unbox = TRUE,
    pretty = pretty
  )
  invisible(path)
}
