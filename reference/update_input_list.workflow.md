# Update the input list of a workflow

This method updates the \`input_list\` of a \`workflow\` object and
optionally writes the updated list to the corresponding inputs file if
the workflow is file-backed.

## Usage

``` r
# S3 method for class 'workflow'
update_input_list(x, new_list, write_file = TRUE, ...)
```

## Arguments

- x:

  The \`workflow\` object to update.

- new_list:

  A named list of input values to update in the workflow.

- write_file:

  Logical; if \`TRUE\`, the updated input list will be written to the
  inputs file if the workflow has associated file paths.

- ...:

  Additional arguments (not used).

## Value

The updated \`workflow\` object with the new input list and optionally
updated parameters.
