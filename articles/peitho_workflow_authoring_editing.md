# Workflow Authoring in PEITHO: Files, Tags, and Programmatic Edits

## Introduction

This vignette explains how to author and modify workflows in PEITHO. It
covers:

- `inputs.json` and `commands.json`
- Input/result tag syntax
- Result selectors
- Programmatic editing APIs

For first-time usage, start with
[`vignette("peitho_workflow_basics")`](https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_basics.md).

## When to Use This Vignette

Use this vignette when you need to define workflows from files, use
tag-based references, or programmatically modify workflow objects.

## Prerequisites

``` r

library(PEITHO)
```

## Workflow File Structure

A file-backed workflow usually includes:

- `inputs.json`
- `commands.json`
- `results.json`
- Optional `functions.R`

### inputs.json

`inputs.json` stores named input values.

``` json
{
  "empire_inputs": "https://en.wikipedia.org/wiki/Roman_Empire\nhttps://en.wikipedia.org/wiki/Achaemenid_Empire",
  "statistics_inputs": "https://en.wikipedia.org/wiki/Coefficient_of_determination"
}
```

### commands.json

`commands.json` stores workflow steps.

``` json
[
  {
    "entry": 1,
    "name": "Split",
    "label": "Split addresses",
    "comments": "",
    "command": "simple_split",
    "args": "x=@#*I*#@empire_inputs@#*I*#@, split=\\n",
    "iteration": "no",
    "samples": 1
  }
]
```

## Argument Tags

Use tags in `args` to reference inputs and prior results:

- Input reference: `@#*I*#@input_name@#*I*#@`
- Result reference: `@#*L*#@step_name@#*L*#@`

Example:

``` json
{
  "entry": 2,
  "name": "get_content",
  "label": "Fetch web content",
  "comments": "",
  "command": "fetch_WebText",
  "args": "url=@#*L*#@Split@#*L*#@",
  "iteration": "auto",
  "samples": 1
}
```

## Result Selectors

You can select subsets from referenced step results by appending
selectors to result tags:

- Single element: `@#*L*#@Split@#*L*#@[2]`
- Range: `@#*L*#@Split@#*L*#@[1:3]`
- Explicit set: `@#*L*#@Split@#*L*#@[[c(1,3)]]`

This is useful when downstream steps should process only part of an
earlier output.

## Load and Edit a Workflow Programmatically

``` r

wf <- new_workflow(workflow_file_paths = workflow_file_paths(path = ""))
```

    ## INFO [2026-06-29 20:14:58] Creating empty results.json file.

### Update inputs

``` r

wf <- update_input_list(
  wf,
  new_list = list(
    empire_inputs = "https://en.wikipedia.org/wiki/Roman_Empire",
    statistics_inputs = "https://en.wikipedia.org/wiki/Coefficient_of_determination"
  )
)
```

### Update a step field

``` r

wf <- update(
  wf,
  step = 2,
  field = "iteration",
  value = "auto"
)
```

### Add and remove steps

``` r

new_step <- new_workflowstep(
  entry = length(wf$steps) + 1,
  command = "identity",
  name = "PassThrough",
  label = "Pass-through",
  args = "x=@#*L*#@get_content@#*L*#@",
  iteration = "no",
  samples = 1
)

wf <- add_step(wf, new_step = new_step)
wf <- remove_step(wf, position = length(wf$steps))
```

### Update custom functions

``` r

wf <- update_functions(
  wf,
  new_functions = "custom_helper <- function(x) x"
)
```

## When to Edit Files Directly vs API

Prefer API methods for routine changes because they keep workflow state
and file-backed updates in sync. Edit JSON files directly only when
doing bulk/manual authoring outside R.

## Summary

You can author workflows through both file definitions and R APIs:

- Define reusable inputs and command chains in JSON
- Reference prior outputs with tags and selectors
- Apply safe, scripted updates with workflow mutation methods

For robust run/recovery patterns, see
[`vignette("peitho_workflow_execution_recovery")`](https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_execution_recovery.md).

## Related Vignettes

- Quickstart (load, run, import/export):
  [`vignette("peitho_workflow_basics")`](https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_basics.md)
- Execution and recovery (errors, run IDs, resume):
  [`vignette("peitho_workflow_execution_recovery")`](https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_execution_recovery.md)
