# Update a workflow state after running a step

This is a generic function to update a workflow state after executing a
step.

## Usage

``` r
update(object, steprun, idx, ...)
```

## Arguments

- object:

  A \`workflowstate\` object.

- steprun:

  A \`workflowsteprun\` object representing the result of the executed
  step.

- idx:

  The index of the step run to update.

- ...:

  Additional arguments (not used).

## Value

The updated \`workflowstate\` object.
