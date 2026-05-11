# Create a new workflow step run object

This object records the execution of a single step within a workflow,
including the step definition, the arguments used, the output or error,
and any additional metadata.

## Usage

``` r
new_workflowsteprun(
  step,
  args,
  output = NULL,
  error = NULL,
  run_id = NULL,
  ...
)
```

## Arguments

- step:

  A \`workflowstep\` object representing the step definition.

- args:

  A list of arguments that were passed to the step's command. Contains
  actual values used during execution (e.g. results of previous steps).

- output:

  List of outputs produced by the step if it executed successfully,
  otherwise list(NULL).

- error:

  List of error conditions if the step encountered errors, otherwise
  list(NULL).

- run_id:

  A unique identifier for the workflow run this step belongs to.

- ...:

  Additional metadata to store with the step run.

## Value

A \`workflowsteprun\` object.
