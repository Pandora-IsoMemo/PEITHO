# Helper to get file paths for PEITHO workflow files

This function constructs full file paths for the workflow files located
in a specified folder. Validation of file existence is performed in
\`workflow_steps_from_files()\`.

## Usage

``` r
workflow_file_paths(
  path = "",
  inputs = "",
  commands = "",
  results = "",
  functions = ""
)
```

## Arguments

- path:

  Path to folder containing workflow files (default: PEITHO example
  folder).

- inputs:

  Name of the inputs file (default: "inputs.json").

- commands:

  Name of the commands file (default: "commands.json").

- results:

  Name of the results summary file (default: "results.json").

- functions:

  Name of the R script file containing custom functions (default:
  "functions.R"). Use \`NULL\` to skip loading a custom functions file.

## Value

A list with full paths to the specified files.
