# Helper to get file paths for PEITHO workflow files

This function constructs full file paths for the workflow files located
in a specified folder. Validation of file existence is performed in
\`extract_workflow_from_files()\`.

## Usage

``` r
workflow_file_paths(
  path = "",
  inputs = "",
  commands = "",
  results = "",
  functions = "",
  config_path = system.file("config", "config.json", package = "PEITHO")
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
  "functions.R").

- config_path:

  Path to configuration JSON file (default: package's config.json).

## Value

A list with full paths to the specified files.
