# Workflow Quickstart in PEITHO: Load, Run, Import, Export

## Introduction

This quickstart shows the fastest path to run a workflow in PEITHO and
share it as a `.peitho` file. You will:

- Load a workflow
- Run selected steps
- Inspect results
- Export and re-import the workflow bundle

For advanced execution topics (resume, robust error handling), see the
vignette `peitho_workflow_execution_recovery`. For workflow authoring
and editing, see `peitho_workflow_authoring_editing`.

## When to Use This Vignette

Use this vignette when you want a fast first success with PEITHO
workflows. It intentionally focuses on the core run/import/export flow
and skips advanced execution and authoring details.

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

You can use the example workflow included with PEITHO by passing an
empty string as the path:

``` r

my_wf <- new_workflow(workflow_file_paths = workflow_file_paths(path = ""))
```

You can inspect what was loaded:

``` r

my_wf
```

    ## <workflow>
    ##   name:   Untitled workflow
    ##   steps:  9
    ##   current:1
    ##   step summary:
    ##    * [1] Split (command: simple_split)
    ##      [2] get_content (command: fetch_WebText)
    ##      [3] paste_urls (command: paste)
    ##      [4] substitute_content (command: gsub)
    ##      [5] split_content (command: simple_split)
    ##      [6] Split_2 (command: simple_split)
    ##      [7] get_content_2 (command: fetch_WebText)
    ##      [8] paste_urls_2 (command: paste)
    ##      [9] paste_content (command: paste)
    ##   path to folder: /home/runner/work/_temp/Library/PEITHO/scripts/peitho_files/example_workflow
    ##   inputs file:   /home/runner/work/_temp/Library/PEITHO/scripts/peitho_files/example_workflow/inputs.json
    ##   commands file: /home/runner/work/_temp/Library/PEITHO/scripts/peitho_files/example_workflow/commands.json
    ##   results file:  /home/runner/work/_temp/Library/PEITHO/scripts/peitho_files/example_workflow/results.json
    ##   functions file:/home/runner/work/_temp/Library/PEITHO/scripts/peitho_files/example_workflow/functions.R
    ##   available fields: $name, $input_list, $steps, $current, $workflow_file_paths, $dots

## Loading a Workflow from a Custom Folder

If you have workflow configuration files (`commands.json`,
`inputs.json`, etc.) stored in a local folder, you can load them
directly by specifying the folder path:

``` r

# define the workflow
my_custom_wf <- new_workflow(
  workflow_file_paths = workflow_file_paths(
    path = file.path("PATH", "TO", "YOUR_CUSTOM_WORKFLOW_FOLDER")
  )
)

# run the workflow, e.g. steps 2 through 4
my_run <- run(my_custom_wf, from = 2, to = 4)

# check the result of the last step
my_run$state$last_result
```

This approach is useful for:

- Testing custom workflows during development
- Loading workflows from local directories without packaging them as zip
  files
- Working with multiple workflow variants stored in different folders

## Exporting a Workflow as a Zip File

You can export the current workflow setup (all relevant files) as a zip
archive (here with the `.peitho` extension):

``` r

zipfile_path <- "./examples/my_workflow.peitho"
save_as_zip(my_wf, file = zipfile_path)
```

    ## INFO [2026-06-29 08:09:10] Creating directory './examples' for saving zip file.

## Running a Workflow

To execute the workflow, use the
[`run()`](https://pandora-isomemo.github.io/PEITHO/reference/run.md)
function. You can specify which steps to run (e.g., from step 1 to 5):

``` r

my_run_1 <- run(my_wf, from = 1, to = 5)
```

    ## INFO [2026-06-29 08:09:10] Starting workflow run with ID: '20260629080910_19cb283f'

    ## INFO [2026-06-29 08:09:10] Running step 1 of 5

    ## INFO [2026-06-29 08:09:10] Parsing arguments for command simple_split

    ## INFO [2026-06-29 08:09:10]   Command 'simple_split': 3 results

    ## INFO [2026-06-29 08:09:10] Running step 2 of 5

    ## INFO [2026-06-29 08:09:10] Parsing arguments for command fetch_WebText

    ## INFO [2026-06-29 08:09:11]   2 sample x iteration runs for command 'fetch_WebText':

    ## INFO [2026-06-29 08:09:11]      2 single results.

    ## INFO [2026-06-29 08:09:12] Running step 3 of 5

    ## INFO [2026-06-29 08:09:12] Parsing arguments for command paste

    ## WARN [2026-06-29 08:09:12] WARNING! Detected list argument(s) for command 'paste', but 'iteration' is set to 'no'.

    ## INFO [2026-06-29 08:09:12]   Command 'paste': single result

    ## INFO [2026-06-29 08:09:12] Running step 4 of 5

    ## INFO [2026-06-29 08:09:12] Parsing arguments for command gsub

    ## INFO [2026-06-29 08:09:12]   Command 'gsub': single result

    ## INFO [2026-06-29 08:09:12] Running step 5 of 5

    ## INFO [2026-06-29 08:09:12] Parsing arguments for command simple_split

    ## INFO [2026-06-29 08:09:12]   Command 'simple_split': 26854 results

After running, you can inspect the results:

``` r

length(my_run_1$state$last_result)
```

    ## [1] 1

``` r

PEITHO:::trunc(my_run_1$state$last_result, n_char = 100)
```

    ## [1] "Rom, \n (27 BC – AD 476)[c]\n\nConstantinopl, \n (330–1453)[d]\n\nOfficial: initially Latin, incr, \nasingly Gr, \n, \n... (26849 more items)"

## Importing a Workflow from a Zip File

To import a workflow from a zip file, first define the directory where
the workflow files will be extracted.

``` r

zipfile_path <- "./examples/my_workflow.peitho"

my_wf_imported <- PEITHO::import_workflow(
  zipfile = zipfile_path,
  extract_dir = "./examples/peitho_import"
)
```

You can now run the imported workflow and inspect the results as before:

``` r

my_run_2 <- run(my_wf_imported, from = 1, to = 4)
```

    ## INFO [2026-06-29 08:09:12] Starting workflow run with ID: '20260629080912_01e47766'

    ## INFO [2026-06-29 08:09:12] Running step 1 of 4

    ## INFO [2026-06-29 08:09:12] Parsing arguments for command simple_split

    ## INFO [2026-06-29 08:09:12]   Command 'simple_split': 3 results

    ## INFO [2026-06-29 08:09:12] Running step 2 of 4

    ## INFO [2026-06-29 08:09:12] Parsing arguments for command fetch_WebText

    ## INFO [2026-06-29 08:09:14]   2 sample x iteration runs for command 'fetch_WebText':

    ## INFO [2026-06-29 08:09:14]      2 single results.

    ## INFO [2026-06-29 08:09:14] Running step 3 of 4

    ## INFO [2026-06-29 08:09:14] Parsing arguments for command paste

    ## WARN [2026-06-29 08:09:14] WARNING! Detected list argument(s) for command 'paste', but 'iteration' is set to 'no'.

    ## INFO [2026-06-29 08:09:14]   Command 'paste': single result

    ## INFO [2026-06-29 08:09:14] Running step 4 of 4

    ## INFO [2026-06-29 08:09:14] Parsing arguments for command gsub

    ## INFO [2026-06-29 08:09:14]   Command 'gsub': single result

``` r

length(my_run_2$state$last_result)
```

    ## [1] 1

``` r

PEITHO:::trunc(my_run_2$state$last_result, n_char = 100)
```

    ## [1] "RomHALLO (27 BC – AD 476)[c]\n\nConstantinoplHALLO (330–1453)[d]\n\nOfficial: initially Latin, incrHALLO ..."

## Summary

PEITHO’s workflow system allows you to:

- Load workflows from the built-in example or custom folder paths
- Load and execute multi-step workflows from configuration files
- Export workflows as zip files for sharing or backup
- Import workflows from zip files
- Inspect and print results from workflow execution

This makes it easy to share, reuse, and adapt complex analysis pipelines
in a reproducible way.

## Next Steps

- Advanced execution and recovery (resume runs, error handling):
  [`vignette("peitho_workflow_execution_recovery")`](https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_execution_recovery.md)
- Authoring and editing workflows (JSON schema, tags, selectors, update
  APIs):
  [`vignette("peitho_workflow_authoring_editing")`](https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_authoring_editing.md)
