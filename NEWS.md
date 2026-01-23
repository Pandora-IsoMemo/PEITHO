# PEITHO 26.01.1

## New Features

- Added `import_workflow()` to streamline workflow import from zip files.
- Updated vignettes and examples to use the new import pattern.

# PEITHO 26.01.0

## New Features

- Implemented export and import of workflows as zip bundles, enabling workflow sharing and reuse (#13).
- Added `save_as_zip()` generic and workflow method for exporting workflows as zip files.
- Enhanced the example workflow with multi-step data processing (including unlist, paste, substitute, and split operations).
- Added a comprehensive vignette demonstrating workflow import/export capabilities.

# PEITHO 25.12.0

## New Features

- Introduced a comprehensive workflow management system:
  - Define, execute, and track multi-step workflows
  - New core classes: `workflow`, `workflowstep`, `workflowstate`, `workflowsteprun`, `operationparam`
  - Robust workflow execution with error handling and result tracking
  - Support for file-based workflow configuration via JSON files

# PEITHO 25.11.0

## New Features

- Added web text fetching capabilities to the PEITHO package, including:
  - A new `WebText` class for representing fetched web content.
  - The `fetch_WebText` function for retrieving and parsing HTML from URLs.
- Implemented comprehensive tests for both the `WebText` class and the web text fetching functionality.