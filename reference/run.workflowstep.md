# Run a workflow step

This function executes a single workflow step, updating the workflow
state with the result or error from the step execution.

## Usage

``` r
# S3 method for class 'workflowstep'
run(
  x,
  state,
  env = NULL,
  step_i = NULL,
  step_idx = NULL,
  input_list = NULL,
  results_path = NULL,
  resume_from_sample = NULL,
  resume_from_iteration = NULL,
  ...
)
```

## Arguments

- x:

  A \`workflowstep\` object representing the step to execute.

- state:

  A \`workflowstate\` object representing the current state of the
  workflow.

- env:

  An environment to look up the command function. Defaults to the
  caller's env.

- step_i:

  The number of the step in the workflow, used for logging purposes.

- step_idx:

  The absolute index of the step in the workflow, used for persistence.

- input_list:

  A list of inputs for argument parsing, loaded from the workflow's
  inputs file.

- results_path:

  An optional path to a results file for intermediate writes.

- resume_from_sample:

  The first sample index still needing execution (NULL = run all).

- resume_from_iteration:

  The first iteration index still needing execution (NULL = run all).

- ...:

  Additional arguments (not used).

## Value

A \`workflowsteprun\` object recording the execution of the step.
