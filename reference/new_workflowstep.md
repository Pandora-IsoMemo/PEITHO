# Create a new workflow step object

This object defines a single step within a workflow, including its
unique identifier, name, operation, parameters, and other metadata.

## Usage

``` r
new_workflowstep(
  id,
  operation,
  name = NULL,
  label = NULL,
  comments = "",
  params = list(),
  loop = "",
  env = parent.frame(),
  ...
)
```

## Arguments

- id:

  An integer identifier for the step.

- operation:

  A character string specifying the name of the function to execute for
  this step, e.g. "strsplit". This function must exist in the loaded
  name space or in a custom script environment.

- name:

  A human-readable name for the step. Defaults to "Step \<id\>".

- label:

  A label for the step, used in UIs. Defaults to the same as \`name\`.

- comments:

  A character string with comments or description for the step.

- params:

  A list of parameters to pass to the operation function.

- loop:

  A character string indicating if the step should be looped over. Can
  be "yes", "no", or "auto".

- env:

  An environment to look up the operation function. Default is the
  parent frame.

- ...:

  Additional metadata to store with the step.

## Value

A \`workflowstep\` object.
