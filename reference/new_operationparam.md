# Create a new operation parameter object

An operation parameter represents a single argument for a step
operation. It stores its position in the argument list, a value, and a
type.

## Usage

``` r
new_operationparam(
  step_id,
  position,
  name = NULL,
  value = "",
  label = "",
  type = c("literal", "input", "result"),
  iteration = c("no", "yes", "auto"),
  loop = NULL,
  selector = NULL,
  ...
)
```

## Arguments

- step_id:

  The ID of the step this parameter belongs to.

- position:

  Integer position of this parameter in the argument list.

- name:

  The name of the parameter (default: \`NULL\` for unnamed).

- value:

  The stored value for this parameter (default: "").

- label:

  A human-readable label for this parameter (default: "").

- type:

  Type of the parameter. One of: - "input" : value comes from user input
  or external input - "result" : value refers to a previous step's
  result - "literal": value is used as-is (a literal argument)

- iteration:

  Iteration behavior for this parameter. One of: - "no" : do not
  iterate - "yes" : always iterate - "auto" : automatically determine
  iteration, iterate if input is a list else not

- loop:

  Deprecated alias for \`iteration\` (kept for backward compatibility).

- selector:

  Optional selector for result references (for example \`2\`, \`1:3\`,
  \`c(1,3)\`).

- ...:

  Additional metadata (optional).

## Value

A \`operationparam\` object.
