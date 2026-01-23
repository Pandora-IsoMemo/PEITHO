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

- For development only, you may use `devtools::load_all()` instead of
  [`library(PEITHO)`](https://pandora-isomemo.github.io/PEITHO/).

## Loading the Example Workflow

You can create a workflow object from your own files by specifying a
path, or use the example workflow included with PEITHO by passing an
empty string as the path:

``` r
my_wf <- new_workflow(workflow_file_paths = workflow_file_paths(path = ""))
```

    ## WARN [2026-01-23 08:22:47] results.json not found in folder '/home/runner/work/_temp/Library/PEITHO/scripts/peitho_files/example_workflow'. Creating empty results file.

    ## INFO [2026-01-23 08:22:47] Parsing command simple_split for step 1

    ## INFO [2026-01-23 08:22:47] Parsing command fetch_WebText for step 2

    ## INFO [2026-01-23 08:22:47] Parsing command unlist for step 3

    ## INFO [2026-01-23 08:22:47] Parsing command paste for step 4

    ## INFO [2026-01-23 08:22:47] Parsing command gsub for step 5

    ## INFO [2026-01-23 08:22:47] Parsing command simple_split for step 6

## Exporting a Workflow as a Zip File

You can export the current workflow setup (all relevant files) as a zip
archive (here with the `.peitho` extension):

``` r
zipfile_path <- "./examples/my_workflow.peitho"
save_as_zip(my_wf, file = zipfile_path)
```

    ## INFO [2026-01-23 08:22:47] Creating directory './examples' for saving zip file.

## Running a Workflow

To execute the workflow, use the
[`run()`](https://pandora-isomemo.github.io/PEITHO/reference/run.md)
function. You can specify which steps to run (e.g., from step 1 to 5):

``` r
my_run_1 <- run(my_wf, from = 1, to = 5)
```

    ## INFO [2026-01-23 08:22:47] Running step 1 of 5

    ## INFO [2026-01-23 08:22:47] Running step 2 of 5

    ## INFO [2026-01-23 08:22:49] Running step 3 of 5

    ## INFO [2026-01-23 08:22:49] Running step 4 of 5

    ## INFO [2026-01-23 08:22:50] Running step 5 of 5

After running, you can inspect the results:

``` r
length(my_run_1$state$last_result)
```

    ## [1] 3

``` r
PEITHO:::trunc(my_run_1$state$last_result, n_char = 100)
```

    ## [1] "Main pagHALLO\\nContHALLOnts\\nCurrHALLOnt HALLOvHALLOnts\\nRandom articlHALLO\\nAbout WikipHALLOdia\\nCo ..., Main pagHALLO\\nContHALLOnts\\nCurrHALLOnt HALLOvHALLOnts\\nRandom articlHALLO\\nAbout WikipHALLOdia\\nCo ..., Main pagHALLO\\nContHALLOnts\\nCurrHALLOnt HALLOvHALLOnts\\nRandom articlHALLO\\nAbout WikipHALLOdia\\nCo ..."

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

    ## INFO [2026-01-23 08:22:50] Parsing command simple_split for step 1

    ## INFO [2026-01-23 08:22:50] Parsing command fetch_WebText for step 2

    ## INFO [2026-01-23 08:22:50] Parsing command unlist for step 3

    ## INFO [2026-01-23 08:22:50] Parsing command paste for step 4

    ## INFO [2026-01-23 08:22:50] Parsing command gsub for step 5

    ## INFO [2026-01-23 08:22:50] Parsing command simple_split for step 6

You can now run the imported (and possibly modified) workflow and
inspect the results as before:

``` r
my_run_2 <- run(my_wf_imported, from = 1, to = 4)
```

    ## INFO [2026-01-23 08:22:50] Running step 1 of 4

    ## INFO [2026-01-23 08:22:50] Running step 2 of 4

    ## INFO [2026-01-23 08:22:52] Running step 3 of 4

    ## INFO [2026-01-23 08:22:52] Running step 4 of 4

``` r
length(my_run_2$state$last_result)
```

    ## [1] 3

``` r
PEITHO:::trunc(my_run_2$state$last_result, n_char = 100)
```

    ## [1] "Main page\\nContents\\nCurrent events\\nRandom article\\nAbout Wikipedia\\nContact us\\nHelp\\nLearn to edi ..., Main page\\nContents\\nCurrent events\\nRandom article\\nAbout Wikipedia\\nContact us\\nHelp\\nLearn to edi ..., Main page\\nContents\\nCurrent events\\nRandom article\\nAbout Wikipedia\\nContact us\\nHelp\\nLearn to edi ..."

## Summary

PEITHO’s workflow system allows you to:

- Load and execute multi-step workflows from configuration files
- Export workflows as zip files for sharing or backup
- Import and modify workflows from zip files
- Inspect and print results from workflow execution

This makes it easy to share, reuse, and adapt complex analysis pipelines
in a reproducible way.
