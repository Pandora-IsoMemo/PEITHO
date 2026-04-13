# Convert a workflow object to commands.json record format

This method converts a \`workflow\` object into a list format suitable
for writing to a \`commands.json\` file, which is used to define the
workflow steps in a structured way. It applies the conversion to each
step in the workflow.

## Usage

``` r
# S3 method for class 'workflow'
as.commands_record(x, ...)
```

## Arguments

- x:

  A \`workflow\` object.

- ...:

  Additional arguments (not used).

## Value

A list summarizing workflow steps for commands.json.
