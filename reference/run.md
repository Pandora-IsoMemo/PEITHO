# Run a workflow step This is a generic function to run a workflow step or an entire workflow.

Run a workflow step This is a generic function to run a workflow step or
an entire workflow.

## Usage

``` r
run(object, state, ...)
```

## Arguments

- object:

  A \`workflowstep\` object or a \`workflow\` object.

- state:

  A \`workflowstate\` object.

- ...:

  Additional arguments (not used).

## Value

A \`workflowsteprun\` object representing the result of executing the
step or a list containing the updated \`workflow\` and \`workflowstate\`
objects.
