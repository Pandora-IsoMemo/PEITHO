# Workflow Execution in PEITHO: Errors, Resume, and Recovery

## Introduction

This vignette covers robust workflow execution patterns in PEITHO. It
focuses on how to:

- Continue runs after failures
- Control stop-on-error behavior
- Reuse run IDs to resume partially completed runs
- Inspect run metadata for troubleshooting

If you are new to PEITHO workflows, start with
[`vignette("peitho_workflow_basics")`](https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_basics.md).

## When to Use This Vignette

Use this vignette when your workflow runs are long-running, error-prone,
or need restart/resume behavior.

## Prerequisites

``` r

library(PEITHO)
```

## Load a Workflow

``` r

wf <- new_workflow(workflow_file_paths = workflow_file_paths(path = ""))
```

## Basic Run and Run Metadata

``` r

run_1 <- run(wf, from = 1, to = 3)
```

    ## INFO [2026-06-29 08:04:01] Starting workflow run with ID: '20260629080401_14acd597'

    ## INFO [2026-06-29 08:04:01] Running step 1 of 3

    ## INFO [2026-06-29 08:04:01] Parsing arguments for command simple_split

    ## INFO [2026-06-29 08:04:01]   Command 'simple_split': 3 results

    ## INFO [2026-06-29 08:04:01] Running step 2 of 3

    ## INFO [2026-06-29 08:04:01] Parsing arguments for command fetch_WebText

    ## INFO [2026-06-29 08:04:02]   2 sample x iteration runs for command 'fetch_WebText':

    ## INFO [2026-06-29 08:04:02]      2 single results.

    ## INFO [2026-06-29 08:04:02] Running step 3 of 3

    ## INFO [2026-06-29 08:04:02] Parsing arguments for command paste

    ## WARN [2026-06-29 08:04:02] WARNING! Detected list argument(s) for command 'paste', but 'iteration' is set to 'no'.

    ## INFO [2026-06-29 08:04:02]   Command 'paste': single result

``` r

run_1$run_id
```

    ## [1] "20260629080401_14acd597"

The `run_id` uniquely identifies a workflow execution and is used for
resume operations.

## Error Handling Strategy

By default,
[`run()`](https://pandora-isomemo.github.io/PEITHO/reference/run.md)
uses `stop_on_error = TRUE`. You can set `stop_on_error = FALSE` to
continue execution and inspect errors afterward.

``` r

run_nonstop <- run(
  wf,
  from = 1,
  to = 5,
  stop_on_error = FALSE
)

run_nonstop$errors
```

Use this mode when you want a full pass and post-run diagnosis, rather
than fail-fast behavior.

## Resume a Run

If a run stops midway, resume using the same `run_id`.

``` r

# First run (example may stop due to an error):
first_try <- run(wf, from = 1, to = 5)

# Resume from saved run ID:
resumed <- run(
  wf,
  from = 1,
  to = 5,
  resume_run_id = first_try$run_id
)
```

`resume_run_id` tells PEITHO to continue from the first unfinished
step/iteration recorded for that run.

## Practical Recovery Workflow

A common pattern for robust execution:

1.  Run with `stop_on_error = TRUE` during normal usage.
2.  Capture `run_id` when an interruption/error occurs.
3.  Fix the issue (input values, custom function, environment, etc.).
4.  Re-run with `resume_run_id` to avoid repeating finished work.

``` r

safe_run <- run(wf, from = 1, to = 5, stop_on_error = TRUE)

# ... investigate/fix root cause ...

safe_run_resumed <- run(
  wf,
  from = 1,
  to = 5,
  resume_run_id = safe_run$run_id,
  stop_on_error = TRUE
)
```

## Troubleshooting Checklist

- Confirm workflow files exist and are readable.
- Ensure command functions are available in the expected environment.
- Verify inputs referenced in command args actually exist in
  `input_list`.
- Check the run object (`$errors`, `$state`, `$run_id`) before retrying.

## Summary

You now have a robust execution workflow:

- Run with explicit error strategy
- Capture and reuse `run_id`
- Resume incomplete runs without reprocessing completed steps

For schema details and editing workflows, continue with
[`vignette("peitho_workflow_authoring_editing")`](https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_authoring_editing.md).

## Related Vignettes

- Quickstart (load, run, import/export):
  [`vignette("peitho_workflow_basics")`](https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_basics.md)
- Authoring and editing (files, tags, selectors, update APIs):
  [`vignette("peitho_workflow_authoring_editing")`](https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_authoring_editing.md)
