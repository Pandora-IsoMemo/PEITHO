# Create a new workflow object

This object represents a workflow, consisting of a sequence of workflow
steps, the current step index, and additional metadata.

## Usage

``` r
new_workflow(
  name = "Untitled workflow",
  steps = list(),
  current = if (length(steps)) 1L else NA_integer_,
  use_peitho_folder = TRUE,
  workflow_file_paths = list(),
  ...
)
```

## Arguments

- name:

  A name for the workflow.

- steps:

  A list of \`workflowstep\` objects defining the steps of the workflow.

- current:

  The index of the current step in the workflow.

- use_peitho_folder:

  Logical; if \`TRUE\`, load steps from PEITHO example folder.

- workflow_file_paths:

  A list of file paths for workflow files (see
  \`workflow_file_paths()\`).

- ...:

  Additional metadata to store with the workflow.

## Value

A \`workflow\` object.
