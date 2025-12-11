
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