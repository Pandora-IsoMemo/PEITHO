# Write content to the functions file of a workflow

This helper function writes the provided content to the functions file
of a workflow, if the workflow is file-backed and has a valid functions
path. If the directory path is NULL or does not exist, the function will
return NULL without performing any write operation.

## Usage

``` r
write_functions_file(dir_path, content)
```

## Arguments

- dir_path:

  The directory path of the workflow. If NULL or non-existent, no write
  will occur.

- content:

  The content to write to the functions file.

## Value

NULL if the directory path is invalid; otherwise, the content is written
to the functions file.
