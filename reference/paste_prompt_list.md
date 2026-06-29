# Paste a list of strings with a prefix and suffix

This function takes a list of strings and concatenates each string with
a specified prefix and suffix. It returns a character vector where each
element is the result of pasting the prefix, the string, and the suffix
together. The function also includes error handling for invalid inputs
and warnings for NA values in the string list.

## Usage

``` r
paste_prompt_list(string_list, prefix, suffix)
```

## Arguments

- string_list:

  A list of strings to be concatenated with the prefix and suffix. If
  \`NULL\`, the function returns an empty character vector.

- prefix:

  A single character string to be prefixed to each element of
  \`string_list\`. Must be a single character string.

- suffix:

  A single character string to be suffixed to each element of
  \`string_list\`. Must be a single character string.

## Value

A character vector where each element is the result of pasting the
prefix, the string from \`string_list\`, and the suffix together. If
\`string_list\` is \`NULL\`, returns an empty character vector. If
\`string_list\` contains NA values, they will be included as "NA" in the
output, and a warning will be issued.
