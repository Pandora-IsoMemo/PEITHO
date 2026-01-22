# Working with Workflows in PEITHO: Import, Export, and Execution

## Introduction

This vignette demonstrates how to use the PEITHO package to manage,
export, import, and execute workflows. The workflow system in PEITHO
allows you to define a sequence of steps (such as data fetching,
processing, or analysis), save and share workflows as zip files, and
re-import or modify them for further use. This guide is aimed at users
with basic R knowledge, but no advanced R expertise is required.

## Prerequisites

- Install the PEITHO package from GitHub:

  ``` r
  devtools::install_github("Pandora-IsoMemo/PEITHO")
  ```

- Load the package:

  ``` r
  library(PEITHO)
  ```

- (For development only, you may use `devtools::load_all()` instead of
  [`library(PEITHO)`](https://pandora-isomemo.github.io/PEITHO/).)

## Loading the Example Workflow

You can create a workflow object from your own files by specifying a
path, or use the example workflow included with PEITHO by passing an
empty string as the path:

``` r
# Create a workflow object from files
# - Set a custom path to the workflow directory, or
# - Use the example included with PEITHO by leaving path empty
my_wf <- new_workflow(
  workflow_file_paths = workflow_file_paths(path = "")
)
```

## Exporting a Workflow as a Zip File

You can export the current workflow setup (all relevant files) as a zip
archive (here with the `.peitho` extension):

``` r
# Save the workflow as a zip file
zipfile_path <- "./my_workflow.peitho"
save_as_zip(my_wf, file = zipfile_path)
```

## Running a Workflow

To execute the workflow, use the
[`run()`](https://pandora-isomemo.github.io/PEITHO/reference/run.md)
function. You can specify which steps to run (e.g., from step 1 to 5):

``` r
# Run the workflow from step 1 to 5
my_run_1 <- run(my_wf, from = 1, to = 5)
```

After running, you can inspect the results:

``` r
# See how many outputs were produced (e.g., 3 outputs from 3 URLs)
length(my_run_1$state$last_result)

# Print the first 100 characters of each result
PEITHO:::trunc(my_run_1$state$last_result, n_char = 100)
```

## Importing and Modifying a Workflow from a Zip File

You can import a workflow from a zip file. Optionally, you can modify
the workflow steps (e.g., in `commands.json` or `inputs.json`) before
running it:

``` r
# Define the directory to extract the workflow files to
extract_dir <- "./inst/scripts/peitho_files/imported"

# Import the workflow bundle from the zip file and extract the files into the extract_dir folder
res <- import_bundle_zip(
  zipfile = zipfile_path,
  extract_dir = extract_dir,
  keep_dir = TRUE
)
```

After importing, you can set up a new workflow object using the
extracted files:

``` r
# Create a workflow from the imported files
my_wf_imported <- new_workflow(
  workflow_file_paths = workflow_file_paths(path = extract_dir)
)
```

You can now run the imported (and possibly modified) workflow:

``` r
# Run the imported workflow from step 1 to 4
my_run_2 <- run(my_wf_imported, from = 1, to = 4)

# See results
length(my_run_2$state$last_result)
PEITHO:::trunc(my_run_2$state$last_result, n_char = 100)
```

## Summary

PEITHO’s workflow system allows you to: - Load and execute multi-step
workflows from configuration files - Export workflows as zip files for
sharing or backup - Import and modify workflows from zip files - Inspect
and print results from workflow execution

This makes it easy to share, reuse, and adapt complex analysis pipelines
in a reproducible way.
