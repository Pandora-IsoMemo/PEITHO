# PEITHO

<!-- badges: start -->
[![R-CMD-check](https://github.com/Pandora-IsoMemo/PEITHO/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/PEITHO/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

TO-DO: create a graphic that displays how workflows work (layers, workflow, state, runs, iteration and samples...)
Add this to a vignette


### Release notes (Changelog)

See the latest release notes in [NEWS.md](./NEWS.md)

### Documentation

https://pandora-isomemo.github.io/PEITHO/

## Quick Example

### Local Installation

```{r, eval=FALSE}
# Add repositories
options(
  repos = c(
    getOption("repos"),
    PANDORA = "https://Pandora-IsoMemo.github.io/drat/",
    INWTlab = "https://inwtlab.github.io/drat/"
  )
)

# Install package
install.packages("PEITHO")

# Load and attach package
library("PEITHO")
```

### Use a workflow from a folder

If you have workflow configuration files (`commands.json`, `inputs.json`, etc.) stored in a local folder, you can load them directly by specifying the folder path:

```{r, eval=FALSE}
# define the workflow
my_custom_wf <- new_workflow(
  workflow_file_paths = workflow_file_paths(
    path = file.path("PATH", "TO", "YOUR_CUSTOM_WORKFLOW_FOLDER")
  )
)

# run the workflow, e.g. steps 2 through 4 
my_run <- run(my_custom_wf, from = 2, to = 4)

# check the result of the last step
my_run$state$last_result
```

This approach is useful for:
- Testing custom workflows during development
- Loading workflows from local directories without packaging them as zip files
- Working with multiple workflow variants stored in different folders

## How to use this Package

For a full description of the usage of the PEITHO package we refer to the [vignette](https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_basics.html). You can find it in the [Documentation](#documentation) above.

## Notes for developers

### Documentation Updates

When adding information to the _help_ sites, _docstrings_ or the _vignette_ of this 
package, please update documentation locally as follows. The documentation of
the main branch is built automatically via github action.

```R
devtools::document() # or CTRL + SHIFT + D in RStudio
devtools::build_site()
```

### Local Docker Container

When testing with a local docker container, please make sure to rebuild the docker image after changes in the R code or dependencies. You can do this from the root of the repository via:

```bash
docker build -t peitho-app:latest .
```

After that, start the container as usual via:

```bash
docker run -p 3838:3838 peitho-app:latest
```

and access the app in your browser at `http://localhost:3838/`. Stop the container with `CTRL + C` in the terminal.