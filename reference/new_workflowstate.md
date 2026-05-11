# Create a new workflow state object

This object keeps track of the state while executing a workflow,
including the initial input, the last result, and the list of executed
step runs.

## Usage

``` r
new_workflowstate(initial_input = NULL, run_id = NULL)
```

## Arguments

- initial_input:

  The initial input provided to the workflow.

- run_id:

  A unique identifier for the workflow run (optional).

## Value

A \`workflowstate\` object.
