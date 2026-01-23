# PEITHO

<!-- badges: start -->
[![R-CMD-check](https://github.com/Pandora-IsoMemo/PEITHO/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/PEITHO/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

### Documenation

- https://pandora-isomemo.github.io/PEITHO/

### Release notes (Changelog):

[See the latest release notes in NEWS.md](./NEWS.md)

## How to use this Package

Refer to the [vignette](https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_basics.html) 
for a description of the usage of the PEITHO package. You can find it in the [Documentation](#documenation) above.

## Notes for developers

When adding information to the _help_ sites, _docstrings_ or the _vignette_ of this 
package, please update documentation locally as follows. The documentation of
the main branch is build automatically via github action.

```R
devtools::document() # or CTRL + SHIFT + D in RStudio
devtools::build_site()
```