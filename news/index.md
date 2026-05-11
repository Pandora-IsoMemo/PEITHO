# Changelog

## PEITHO 26.04.2

### Updates

- Updated run.workflowstep() to always store output/error as lists
- Extended test coverage to assert list-shaped output/error and
  list-shaped last_result
- Added run_id to workflow run and state objects

## PEITHO 26.04.1

### Bug Fixes

- Fixed loading of custom functions (from functions.R) into an
  environment for the “Browse functions” modal and adjusted save
  messaging for functions.R.
- Fixed how custom workflow functions are loaded and surfaced in the
  Shiny UI and workflow execution, with the goal of picking up updated
  functions more reliably.

## PEITHO 26.04.0

### New Features

- Added a “Folders & Files” tab to the Shiny app, enabling users to
  browse workflow directories, view and edit supported workflow files,
  and inspect package defaults and available functions. (#29)

### Updates

- Result selector support: extended workflow tag parsing to support
  optional selectors (e.g., \[n\], \[a:b\], \[c(…)\]), including
  `extract_result_ref`, new selector tag regex handling, and selector
  application during execution when resolving results from previous
  steps.

## PEITHO 26.03.1

### New Features

- Added
  [`add_step()`](https://pandora-isomemo.github.io/PEITHO/reference/add_step.md)
  /
  [`remove_step()`](https://pandora-isomemo.github.io/PEITHO/reference/remove_step.md)
  S3 generics + workflow methods and wire them into the Shiny workflow
  table UI (#28)
- Add configurable workflow validation (`error_on_warn`) and additional
  validation checks (unique steps, numeric entries, required
  inputs/steps).
- Introduced
  [`as.commands_record()`](https://pandora-isomemo.github.io/PEITHO/reference/as.commands_record.md)
  generic + methods and update `commands.json` writing behavior; update
  docs and example `commands.json`.

## PEITHO 26.03.0

### New Features

- Refactored workflowstep from operation/params to command/args and
  added required-field tracking for input/result tags.
- Updated workflow file-loading helpers and associated tests/docs;
  adjusted WebText fetching to a structural extraction approach.
- Added Shiny table modules (workflow/inputs/results) including option
  for editing step fields and input values, and the option to display
  full result content when clicked (#21).

## PEITHO 26.02.3

### New Features

- Integrated workflow import and export functionality into the PEITHO
  Shiny application, enabling users to upload workflows from zip files
  and download them for sharing or backup. (#21)
- Added a centralized YAML configuration file (inst/config.yaml) to
  manage app settings such as allowed file types and default import
  sources.

## PEITHO 26.02.2

### Updates

- Removed ‘host’ argument from
  [`startApplication()`](https://pandora-isomemo.github.io/PEITHO/reference/startApplication.md)
  function, as it is no longer needed.

## PEITHO 26.02.1

### Bug Fixes

- Fixed missing argument `host` in
  [`startApplication()`](https://pandora-isomemo.github.io/PEITHO/reference/startApplication.md)
  function, allowing specification of the host address when starting the
  Shiny app.

## PEITHO 26.02.0

### New Features

- Introduced
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) S3
  methods for workflows, workflow runs, and workflow states, plus a new
  `extract_inputs()` helper.
- Extended the Shiny app to build and execute an example workflow with
  progress tracking and tabular result views (#21).

## PEITHO 26.01.3

### New Features

- Extended the example workflow to demonstrate **sub-workflows**,
  allowing sequential execution of multiple workflow segments (#14)
- Sub-workflows can now **reference results from earlier steps
  explicitly**, enabling more complex, non-linear workflows.
- Improved logging for loop execution, including clearer warnings when
  operations produce list-type results.

### Bug Fixes

- Fixed a bug where result parameters always resolved to the **last
  executed step**, ignoring the referenced step label.
- Workflow execution now uses **cached state lookups** to resolve result
  parameters by step name (and ID), ensuring correct dependency handling
  across steps.

## PEITHO 26.01.2

### Updates

- Converted a warning into an informational message when creating a new
  `results.json` file if it does not yet exist.

## PEITHO 26.01.1

### New Features

- Added
  [`import_workflow()`](https://pandora-isomemo.github.io/PEITHO/reference/import_workflow.md)
  to streamline workflow import from zip files.
- Updated vignettes and examples to use the new import pattern.

## PEITHO 26.01.0

### New Features

- Implemented export and import of workflows as zip bundles, enabling
  workflow sharing and reuse (#13).
- Added
  [`save_as_zip()`](https://pandora-isomemo.github.io/PEITHO/reference/save_as_zip.md)
  generic and workflow method for exporting workflows as zip files.
- Enhanced the example workflow with multi-step data processing
  (including unlist, paste, substitute, and split operations).
- Added a comprehensive vignette demonstrating workflow import/export
  capabilities.

## PEITHO 25.12.0

### New Features

- Introduced a comprehensive workflow management system:
  - Define, execute, and track multi-step workflows
  - New core classes: `workflow`, `workflowstep`, `workflowstate`,
    `workflowsteprun`, `operationparam`
  - Robust workflow execution with error handling and result tracking
  - Support for file-based workflow configuration via JSON files

## PEITHO 25.11.0

### New Features

- Added web text fetching capabilities to the PEITHO package, including:
  - A new `WebText` class for representing fetched web content.
  - The `fetch_WebText` function for retrieving and parsing HTML from
    URLs.
- Implemented comprehensive tests for both the `WebText` class and the
  web text fetching functionality.
