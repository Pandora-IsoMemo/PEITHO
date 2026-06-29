# Update a workflow step with a new value

This function updates a specific field of a \`workflowstep\` object with
a new value. It is used to modify step details such as name, label,
comments, command, parameters, or iteration settings.

## Usage

``` r
# S3 method for class 'workflow'
update(x, step, field, value, ...)
```

## Arguments

- x:

  A \`workflow\` object to update.

- step:

  The index of the step to update.

- field:

  The name of the field to update (one of "name", "label", "comments",
  "command", "args", "iteration", "loop")

- value:

  The new value to assign to the specified field.

- ...:

  Additional arguments (not used).

## Value

The updated \`workflow\` object.
