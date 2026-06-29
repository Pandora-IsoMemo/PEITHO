# Convert a workflow to graph tables

This method converts a \`workflow\` object into graph tables (nodes and
edges) for visualization or analysis. It extracts relevant information
from each workflow step to create a structured representation of the
workflow's structure and dependencies. Nodes represent individual steps
with their attributes, while edges represent the dependencies between
steps based on the \`required_steps\` field. Additionally, input nodes
are created from the workflow's \`input_list\`, and edges representing
\`required_inputs\` relationships are added.

## Usage

``` r
# S3 method for class 'workflow'
as.graph_tables(x, ...)
```

## Arguments

- x:

  The workflow object to convert.

- ...:

  Additional arguments passed to methods.

## Value

A list containing graph tables representing the workflow structure.
\`nodes\` is a data.frame with columns: \`id\`, \`name\`, \`label\`,
\`command\`, \`entry\`, \`order\`, \`type\` (either "step" or "input").
\`edges\` is a data.frame with columns: \`from\`, \`to\`, \`rel\`
(either "required_steps" or "required_inputs").
