# Recursively clean a list by removing NULL values This function takes a list and recursively removes any NULL values from it. If the input is not a list, it returns the input unchanged. If the input is NULL, it returns NULL.

Recursively clean a list by removing NULL values This function takes a
list and recursively removes any NULL values from it. If the input is
not a list, it returns the input unchanged. If the input is NULL, it
returns NULL.

## Usage

``` r
prune_nulls(x)
```

## Arguments

- x:

  A list to be cleaned, or any other R object.

## Value

A cleaned list with all NULL values removed, or the original input if it
is not a list.
