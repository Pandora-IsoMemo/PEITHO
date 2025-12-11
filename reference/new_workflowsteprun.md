# Create a new workflow step run object

This object records the execution of a single step within a workflow,
including the step definition, the arguments used, the output or error,
and any additional metadata.

## Usage

``` r
new_workflowsteprun(step, args, output = NULL, error = NULL, ...)
```

## Arguments

- step:

  A \`workflowstep\` object representing the step definition.

- args:

  A list of arguments that were passed to the step's operation. Contains
  actual values used during execution (e.g. results of previous steps).

- output:

  The output produced by the step, if successful.

- error:

  An error object if the step failed, otherwise \`NULL\`.

- ...:

  Additional metadata to store with the step run.

## Value

A \`workflowsteprun\` object.
