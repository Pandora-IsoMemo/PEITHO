# Run the entire workflow

Run the entire workflow

## Usage

``` r
# S3 method for class 'workflow'
run(x, state = list(), from = 1L, to = length(x$steps), env = NULL, ...)
```

## Arguments

- x:

  A \`workflow\` object.

- state:

  A \`workflowstate\` object representing the initial state.

- from:

  An integer index of the step to start from.

- to:

  An integer index of the step to end at.

- env:

  An environment to look up command functions. Defaults to \`NULL\`,
  which uses each step's own env or the caller's env.

- ...:

  Additional arguments passed to \`run.workflowstep()\`.

## Value

A list containing the final workflow, state, and results of each step.
