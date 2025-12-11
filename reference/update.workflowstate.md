# Add a workflow step run to the workflow state

This function appends a \`workflowsteprun\` object to the list of
executed step runs

## Usage

``` r
# S3 method for class 'workflowstate'
update(object, steprun, idx, ...)
```

## Arguments

- object:

  A \`workflowstate\` object.

- steprun:

  A \`workflowsteprun\` object to add.

- idx:

  The index at which to add the step run.

- ...:

  Additional arguments (not used).

## Value

The updated \`workflowstate\` object.
