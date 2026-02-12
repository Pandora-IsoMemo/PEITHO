# Changelog

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
  [`extract_inputs()`](https://pandora-isomemo.github.io/PEITHO/reference/extract_inputs.md)
  helper.
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
