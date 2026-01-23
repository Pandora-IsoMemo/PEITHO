# Extract workflow steps from files in a folder

Extract workflow steps from files in a folder

## Usage

``` r
extract_workflow_from_files(workflow_file_paths, show_functions_path = TRUE)
```

## Arguments

- workflow_file_paths:

  A list of file paths for workflow files (see
  \`workflow_file_paths()\`). Default is the package's \`peitho_files\`
  folder.

- show_functions_path:

  Logical, whether to show the path of the loaded script file.

## Value

A list of \`workflowstep\` objects.
