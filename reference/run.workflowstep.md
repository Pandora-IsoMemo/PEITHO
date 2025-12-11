# Run a workflow step

This function executes a single workflow step, updating the workflow
state with the result or error from the step execution.

## Usage

``` r
# S3 method for class 'workflowstep'
run(object, state, env = NULL, ...)
```

## Arguments

- object:

  A \`workflowstep\` object representing the step to execute.

- state:

  A \`workflowstate\` object representing the current state of the
  workflow.

- env:

  An environment to look up the operation function. Defaults to the
  step's own env or the caller's env.

- ...:

  Additional arguments (not used).

## Value

A \`workflowsteprun\` object recording the execution of the step.
