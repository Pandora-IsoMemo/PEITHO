# Generate letter combinations

This function generates all possible combinations of lowercase letters
of a specified length, starting from a given string and ending at
another string. The combinations are generated in lexicographical order.
The function validates the input parameters to ensure they are
appropriate for generating the desired combinations.

## Usage

``` r
generate_letter_combinations(n_letters = 3, start = NULL, stop = NULL)
```

## Arguments

- n_letters:

  An integer specifying the number of letters in each combination. Must
  be a positive integer.

- start:

  A character string specifying the starting combination. Must be of
  length \`n_letters\` and consist of lowercase letters. If \`NULL\`,
  defaults to a string of 'a's (e.g., "aaa" for \`n_letters = 3\`).

- stop:

  A character string specifying the ending combination. Must be of
  length \`n_letters\` and consist of lowercase letters. If \`NULL\`,
  defaults to a string of 'z's (e.g., "zzz" for \`n_letters = 3\`).

## Value

A character vector containing all combinations of letters from \`start\`
to \`stop\`, inclusive, in lexicographical order.
