# Workflow run object

This object represents a completed run of a workflow, containing the
workflow definition and the final state after execution.

## Usage

``` r
new_workflowrun(workflow, state, run_id = NULL)
```

## Arguments

- workflow:

  A \`workflow\` object.

- state:

  A \`workflowstate\` object.

- run_id:

  A unique identifier for the workflow run (optional).

## Value

A \`workflowrun\` object.
