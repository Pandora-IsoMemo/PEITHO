# Update the functions file of a workflow

This method updates the content of the functions file associated with a
\`workflow\` object. If the workflow is file-backed, the updated content
will be written to the functions file.

## Usage

``` r
# S3 method for class 'workflow'
update_functions(x, new_functions, ...)
```

## Arguments

- x:

  The \`workflow\` object to update.

- new_functions:

  new functions

- ...:

  Additional arguments (not used).

## Value

The updated \`workflow\` object with the new functions file content.
