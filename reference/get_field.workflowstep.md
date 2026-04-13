# Get a specific field from a workflowstep

Get a specific field from a workflowstep

## Usage

``` r
# S3 method for class 'workflowstep'
get_field(x, field, with_map_field = TRUE, ...)
```

## Arguments

- x:

  A \`workflowstep\` object.

- field:

  The name of the field to retrieve (e.g., "name", "comments").

- with_map_field:

  Logical, whether to map the field name using \`map_field()\`. Defaults
  to \`TRUE\`.

- ...:

  Additional arguments (not used).

## Value

The value of the specified field from the workflowstep.
