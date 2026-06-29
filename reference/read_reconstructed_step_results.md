# Read and reconstruct per-step results from the results JSON file

Reads the flat results JSON file from disk and reconstructs the legacy
per-step summary format via \`reconstruct_step_results()\`. Use this as
the compatibility entry-point for consumers that expect one result entry
per workflow step (e.g. resume logic, export helpers).

## Usage

``` r
read_reconstructed_step_results(
  path_to_folder,
  results_file = "results_summary.json",
  run_id = NULL
)
```

## Arguments

- path_to_folder:

  Path to the workflow folder containing the results file.

- results_file:

  Name of the results JSON file (default: \`"results_summary.json"\`).

- run_id:

  Optional character string. When supplied only records matching this
  run identifier are included.

## Value

A list with one element per workflow step in the legacy per-step summary
format. See \`reconstruct_step_results()\` for details.
