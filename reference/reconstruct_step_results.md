# Reconstruct legacy per-step results from flat iteration records

Converts the flat list of result records (written by the intermediate
persistence layer) back into the legacy per-step summary format that
consumers such as the Shiny results table expect. For looped steps the
individual \`iteration_result\` records are collected in order and their
results are assembled into a list; for non-looped steps the single
\`step_result\` record is returned as-is.

## Usage

``` r
reconstruct_step_results(records, run_id = NULL)
```

## Arguments

- records:

  A list of result records as read from the results JSON file.

- run_id:

  Optional character string. When supplied only records matching this
  run identifier are included.

## Value

A list with one element per workflow step, each element being a named
list with fields \`run_id\`, \`entry\`, \`name\`, \`label\`, \`result\`,
and \`errors\` — matching the legacy per-step summary format.
