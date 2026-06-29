# Determine where to resume a partially completed workflow run

Reads the flat results records and returns the step index and iteration
index at which execution should continue. A step is considered finished
when a \`step_result\` or \`step_summary\` record with \`status =
"finished"\` exists for that step. For a looped step that started but
never finished, \`resume_iteration\` is set to \`max(completed
iteration_id) + 1\`.

## Usage

``` r
get_resume_cursor(
  run_id,
  path_to_folder,
  results_file = "results_summary.json"
)
```

## Arguments

- run_id:

  Character. The run identifier to search for.

- path_to_folder:

  Path to the workflow folder.

- results_file:

  Name of the results JSON file (default: \`"results_summary.json"\`).

## Value

A list with three elements:

- \`resume_step\`:

  Integer. Index of the first step to execute.

- \`resume_iteration\`:

  Integer or \`NULL\`. For a looped step that was partially completed,
  the first iteration index that still needs to run. \`NULL\` when no
  iterations have been recorded yet.

- \`resume_sample\`:

  Integer or \`NULL\`. For sampled runs, the sample index for the first
  unfinished pair. \`NULL\` for legacy iteration-only rows.
