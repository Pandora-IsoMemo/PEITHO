# Create a new workflow step object

This object defines a single step within a workflow, including its
unique identifier, name, command, parameters, and other metadata.

## Usage

``` r
new_workflowstep(
  entry,
  command,
  name = NULL,
  label = NULL,
  comments = "",
  args = "",
  loop = "auto",
  env = parent.frame(),
  ...
)
```

## Arguments

- entry:

  An integer representing the step's position in the workflow. This
  should be unique for each step.

- command:

  A character string specifying the name of the function to execute for
  this step, e.g. "strsplit". This function must exist in the loaded
  name space or in a custom script environment.

- name:

  A human-readable name for the step. Defaults to "Step \<entry\>".

- label:

  A label for the step, used in UIs. Defaults to the same as \`name\`.

- comments:

  A character string with comments or description for the step.

- args:

  The original argument string from the workflow file, for reference.

- loop:

  A character string indicating if the step should be looped over. Can
  be "yes", "no", or "auto".

- env:

  An environment to look up the command function. Default is the parent
  frame.

- ...:

  Additional metadata to store with the step.

## Value

A \`workflowstep\` object.
