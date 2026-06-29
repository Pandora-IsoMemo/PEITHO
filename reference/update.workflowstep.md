# Update a workflow step

This method allows updating specific fields of a \`workflowstep\`
object, such as its name, label, comments, command, parameters, or loop
configuration.

## Usage

``` r
# S3 method for class 'workflowstep'
update(x, workflow_file_paths, value, field, with_map_field = TRUE, ...)
```

## Arguments

- x:

  The \`workflowstep\` object to update.

- workflow_file_paths:

  The paths to the workflow files, used for updating related files.

- value:

  The new value to assign to the specified field.

- field:

  The name of the field to update. Must be one of "name", "label",
  "comments", "command", "args", "iteration", "samples", or "loop".

- with_map_field:

  Logical, whether to map the field name using \`map_field()\`. Defaults
  to \`TRUE\`.

- ...:

  Additional arguments (not used).

## Value

The updated \`workflowstep\` object.
