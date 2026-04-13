# Convert a workflowstate to a data frame

This function summarizes the workflow state by converting the list of
step runs into a data frame. Truncation of error and output fields can
be controlled via \`max_char\` and \`max_items\` passed through \`...\`.

## Usage

``` r
# S3 method for class 'workflowstate'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A \`workflowstate\` object.

- row.names:

  \`NULL\` or a character vector giving the row names for the data
  frame.

- optional:

  Logical, if \`TRUE\`, column names are not syntactically adjusted.

- ...:

  Additional arguments. Supports \`max_char\` (default: 50) and
  \`max_items\` (default: 5) to control truncation of error and output
  fields.

## Value

A data frame summarizing the workflow state.
