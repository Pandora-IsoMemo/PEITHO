# Create a new workflow object

This object represents a workflow, consisting of a sequence of workflow
steps, the current step index, and additional metadata.

## Usage

``` r
new_workflow(
  name = "Untitled workflow",
  workflow_file_paths = list(),
  use_peitho_folder = TRUE,
  input_list = list(),
  steps = list(),
  current = if (length(steps)) 1L else NA_integer_,
  error_on_warn = FALSE,
  ...
)
```

## Arguments

- name:

  A name for the workflow.

- workflow_file_paths:

  A list of file paths for workflow files (see
  \`workflow_file_paths()\`).

- use_peitho_folder:

  Logical; if \`TRUE\`, load steps from PEITHO example folder. If
  \`FALSE\`, use provided \`steps\` and ignore \`workflow_file_paths\`,
  useful for testing.

- input_list:

  A list of inputs for the workflow steps.

- steps:

  A list of \`workflowstep\` objects defining the steps of the workflow.

- current:

  The index of the current step in the workflow.

- error_on_warn:

  Logical; if \`TRUE\`, validation issues will raise errors. If
  \`FALSE\`, they will raise warnings instead.

- ...:

  Additional metadata to store with the workflow.

## Value

A \`workflow\` object.
