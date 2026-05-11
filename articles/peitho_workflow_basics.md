# Working with Workflows in PEITHO: Import, Export, and Execution

## Introduction

This vignette demonstrates how to use the PEITHO package to manage,
export, import, and execute workflows. The workflow system in PEITHO
allows you to define a sequence of steps (such as data fetching,
processing, or analysis), save and share workflows as zip files, and
re-import or modify them for further use. This guide is aimed at users
with basic R knowledge, but no advanced R expertise is required.

## Prerequisites

- Install the PEITHO package from GitHub if not already installed:

  `devtools::install_github("Pandora-IsoMemo/PEITHO")`

- Load the package:

``` r

library(PEITHO)
```

- For development only, you may use
  [`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)
  instead of
  [`library(PEITHO)`](https://pandora-isomemo.github.io/PEITHO/).

## Loading the Example Workflow

You can create a workflow object from your own files by specifying a
path, or use the example workflow included with PEITHO by passing an
empty string as the path:

``` r

my_wf <- new_workflow(workflow_file_paths = workflow_file_paths(path = ""))
```

    ## INFO [2026-05-11 14:29:45] Creating empty results.json file.

## Exporting a Workflow as a Zip File

You can export the current workflow setup (all relevant files) as a zip
archive (here with the `.peitho` extension):

``` r

zipfile_path <- "./examples/my_workflow.peitho"
save_as_zip(my_wf, file = zipfile_path)
```

    ## INFO [2026-05-11 14:29:45] Creating directory './examples' for saving zip file.

## Running a Workflow

To execute the workflow, use the
[`run()`](https://pandora-isomemo.github.io/PEITHO/reference/run.md)
function. You can specify which steps to run (e.g., from step 1 to 5):

``` r

my_run_1 <- run(my_wf, from = 1, to = 5)
```

    ## INFO [2026-05-11 14:29:45] Starting workflow run with ID: '20260511142945_19cb283f'

    ## INFO [2026-05-11 14:29:45] Running step 1 of 5

    ## INFO [2026-05-11 14:29:45] Parsing arguments for command simple_split

    ## INFO [2026-05-11 14:29:45]   Command 'simple_split': 3 results

    ## INFO [2026-05-11 14:29:45] Running step 2 of 5

    ## INFO [2026-05-11 14:29:45] Parsing arguments for command fetch_WebText

    ## INFO [2026-05-11 14:29:47]   2 loop iterations for command 'fetch_WebText':

    ## INFO [2026-05-11 14:29:47]      2 single results.

    ## INFO [2026-05-11 14:29:47] Running step 3 of 5

    ## INFO [2026-05-11 14:29:47] Parsing arguments for command paste

    ## WARN [2026-05-11 14:29:47] WARNING! Detected list argument(s) for command 'paste', but 'loop' is set to 'no'.

    ## INFO [2026-05-11 14:29:47]   Command 'paste': single result

    ## INFO [2026-05-11 14:29:47] Running step 4 of 5

    ## INFO [2026-05-11 14:29:47] Parsing arguments for command gsub

    ## INFO [2026-05-11 14:29:47]   Command 'gsub': single result

    ## INFO [2026-05-11 14:29:47] Running step 5 of 5

    ## INFO [2026-05-11 14:29:47] Parsing arguments for command simple_split

    ## INFO [2026-05-11 14:29:47]   Command 'simple_split': 31356 results

After running, you can inspect the results:

``` r

length(my_run_1$state$last_result)
```

    ## [1] 1

``` r

PEITHO:::trunc(my_run_1$state$last_result, n_char = 100)
```

    ## [1] "Toggl, \n th, \n tabl, \n of cont, \nnts Roman Empir, \n... (31351 more items)"

## Importing and Modifying a Workflow from a Zip File

To import a workflow from a zip file, first define the directory where
the workflow files will be extracted. You may also modify the workflow
steps (for example, by editing `commands.json` or `inputs.json` in the
extracted folder) before running it.

``` r

zipfile_path <- "./examples/my_workflow.peitho"

my_wf_imported <- PEITHO::import_workflow(
  zipfile = zipfile_path,
  extract_dir = "./examples/peitho_import"
)
```

You can now run the imported (and possibly modified) workflow and
inspect the results as before:

``` r

my_run_2 <- run(my_wf_imported, from = 1, to = 4)
```

    ## INFO [2026-05-11 14:29:48] Starting workflow run with ID: '20260511142948_01e47766'

    ## INFO [2026-05-11 14:29:48] Running step 1 of 4

    ## INFO [2026-05-11 14:29:48] Parsing arguments for command simple_split

    ## INFO [2026-05-11 14:29:48]   Command 'simple_split': 3 results

    ## INFO [2026-05-11 14:29:48] Running step 2 of 4

    ## INFO [2026-05-11 14:29:48] Parsing arguments for command fetch_WebText

    ## INFO [2026-05-11 14:29:49]   2 loop iterations for command 'fetch_WebText':

    ## INFO [2026-05-11 14:29:49]      2 single results.

    ## INFO [2026-05-11 14:29:49] Running step 3 of 4

    ## INFO [2026-05-11 14:29:49] Parsing arguments for command paste

    ## WARN [2026-05-11 14:29:49] WARNING! Detected list argument(s) for command 'paste', but 'loop' is set to 'no'.

    ## INFO [2026-05-11 14:29:49]   Command 'paste': single result

    ## INFO [2026-05-11 14:29:49] Running step 4 of 4

    ## INFO [2026-05-11 14:29:49] Parsing arguments for command gsub

    ## INFO [2026-05-11 14:29:49]   Command 'gsub': single result

``` r

length(my_run_2$state$last_result)
```

    ## [1] 1

``` r

PEITHO:::trunc(my_run_2$state$last_result, n_char = 100)
```

    ## [1] "TogglHALLO thHALLO tablHALLO of contHALLOnts Roman EmpirHALLO 188 languagHALLOs Acèh Afrikaans አማርኛ  ..."

## Summary

PEITHO’s workflow system allows you to:

- Load and execute multi-step workflows from configuration files
- Export workflows as zip files for sharing or backup
- Import and modify workflows from zip files
- Inspect and print results from workflow execution

This makes it easy to share, reuse, and adapt complex analysis pipelines
in a reproducible way.
