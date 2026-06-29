# Read existing iteration records for a specific step and run

Returns detail records (\`iteration_result\` and \`sample_result\`)
ordered by \`(iteration_id, sample_id)\`. Used by \`run.workflowstep()\`
to reconstruct skipped-iteration outputs when resuming a partially
completed looped step.

## Usage

``` r
read_iteration_records_for_step(
  run_id,
  step,
  path_to_folder,
  results_file = "results_summary.json"
)
```

## Arguments

- run_id:

  Character. The run identifier.

- step:

  Integer. The step index.

- path_to_folder:

  Path to the workflow folder.

- results_file:

  Name of the results JSON file (default: \`"results_summary.json"\`).

## Value

A list of matching records, each a named list, ordered by
\`iteration_id\`.
