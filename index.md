# PEITHO

### Documenation

- <https://pandora-isomemo.github.io/PEITHO/>

### Release notes (Changelog):

[See the latest release notes in
NEWS.md](https://pandora-isomemo.github.io/PEITHO/NEWS.md)

## How to use this Package

Refer to the
[vignette](https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_basics.html)
for a description of the usage of the PEITHO package. You can find it in
the [Documentation](#documenation) above.

## Notes for developers

When adding information to the *help* sites, *docstrings* or the
*vignette* of this package, please update documentation locally as
follows. The documentation of the main branch is build automatically via
github action.

``` r
devtools::document() # or CTRL + SHIFT + D in RStudio
devtools::build_site()
```
