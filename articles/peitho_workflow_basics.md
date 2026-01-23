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
my_wf <- new_workflow(
  workflow_file_paths = workflow_file_paths(path = "")
)
```

## Exporting a Workflow as a Zip File

You can export the current workflow setup (all relevant files) as a zip
archive (here with the `.peitho` extension):

``` r
zipfile_path <- "./my_workflow.peitho"
save_as_zip(my_wf, file = zipfile_path)
```

## Running a Workflow

To execute the workflow, use the
[`run()`](https://pandora-isomemo.github.io/PEITHO/reference/run.md)
function. You can specify which steps to run (e.g., from step 1 to 5):

``` r
my_run_1 <- run(my_wf, from = 1, to = 5)
```

After running, you can inspect the results:

``` r
length(my_run_1$state$last_result)

PEITHO:::trunc(my_run_1$state$last_result, n_char = 100)
```

## Importing and Modifying a Workflow from a Zip File

To import a workflow from a zip file, first define the directory where
the workflow files will be extracted. You may also modify the workflow
steps (for example, by editing `commands.json` or `inputs.json` in the
extracted folder) before running it.

``` r
extract_dir <- "./inst/scripts/peitho_files/imported"

res <- import_bundle_zip(
  zipfile = zipfile_path,
  extract_dir = extract_dir,
  keep_dir = TRUE
)
```

Once imported, set up a new workflow object using the extracted files:

``` r
my_wf_imported <- new_workflow(
  workflow_file_paths = workflow_file_paths(path = extract_dir)
)
```

You can now run the imported (and possibly modified) workflow and
inspect the results as before:

``` r
my_run_2 <- run(my_wf_imported, from = 1, to = 4)

length(my_run_2$state$last_result)
PEITHO:::trunc(my_run_2$state$last_result, n_char = 100)
```

## Summary

PEITHO’s workflow system allows you to:

- Load and execute multi-step workflows from configuration files
- Export workflows as zip files for sharing or backup
- Import and modify workflows from zip files
- Inspect and print results from workflow execution

This makes it easy to share, reuse, and adapt complex analysis pipelines
in a reproducible way.
