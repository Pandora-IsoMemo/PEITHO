# Run the entire workflow

Run the entire workflow

## Usage

``` r
# S3 method for class 'workflow'
run(
  object,
  state = list(),
  from = 1L,
  to = length(object$steps),
  env = NULL,
  ...
)
```

## Arguments

- object:

  A \`workflow\` object.

- state:

  A \`workflowstate\` object representing the initial state.

- from:

  An integer index of the step to start from.

- to:

  An integer index of the step to end at.

- env:

  An environment to look up operation functions. Defaults to \`NULL\`,
  which uses each step's own env or the caller's env.

- ...:

  Additional arguments passed to \`run.workflowstep()\`.

## Value

A list containing the final workflow, state, and results of each step.
