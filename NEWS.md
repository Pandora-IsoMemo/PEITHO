# PEITHO 26.01.0

## Updates

- Extended the example `workflow` in the `commands.json` file to include a more complex scenario:
  - After splitting web adresses and fetching urls, the workflow is now also
    - unlisting the results
    - pasting the content of the fetched URLs into a single string
    - examplarily substituting the pattern "e" into the string "HALLO" and
    - performing a split operation at the pattern "HALLO" on the resulting string.
  - This showcases the flexibility of the workflow system and its ability to chain multiple operations together.

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