# Convert a workflowstep to commands.json record format

This method converts a \`workflowstep\` object into a list format
suitable for writing to a \`commands.json\` file, which is used to
define the workflow steps in a structured way.

## Usage

``` r
# S3 method for class 'workflowstep'
as.commands_record(x, ...)
```

## Arguments

- x:

  A \`workflowstep\` object.

- ...:

  Additional arguments (not used).

## Value

A list representing the workflow step for commands.json.
