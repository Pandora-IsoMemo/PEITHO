# PEITHO

### Documenation

<https://pandora-isomemo.github.io/PEITHO/>

### Release notes (Changelog)

See the latest release notes in
[NEWS.md](https://pandora-isomemo.github.io/PEITHO/NEWS.md)

## How to use this Package

Refer to the
[vignette](https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_basics.html)
for a description of the usage of the PEITHO package. You can find it in
the [Documentation](#documenation) above.

## Notes for developers

### Documentation Updates

When adding information to the *help* sites, *docstrings* or the
*vignette* of this package, please update documentation locally as
follows. The documentation of the main branch is build automatically via
github action.

``` r

devtools::document() # or CTRL + SHIFT + D in RStudio
devtools::build_site()
```

### Local Docker Container

When testing with a local docker container, please make sure to rebuild
the docker image after changes in the R code or dependencies. You can do
this from the root of the repository via:

``` bash
docker build -t peitho-app:latest .
```

After that, start the container as usual via:

``` bash
docker run -p 3838:3838 peitho-app:latest
```

and access the app in your browser at `http://localhost:3838/`. Stop the
container with `CTRL + C` in the terminal.
