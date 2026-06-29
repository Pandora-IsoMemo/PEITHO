# PEITHO

<!-- badges: start -->
[![R-CMD-check](https://github.com/Pandora-IsoMemo/PEITHO/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/PEITHO/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/Pandora-IsoMemo/PEITHO/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/PEITHO/actions/workflows/pkgdown.yaml)
[![docker-publish](https://github.com/Pandora-IsoMemo/PEITHO/actions/workflows/docker-publish.yml/badge.svg)](https://github.com/Pandora-IsoMemo/PEITHO/actions/workflows/docker-publish.yml)
<!-- badges: end -->

### Release notes (Changelog)

See the latest release notes in [NEWS.md](./NEWS.md)

### Documentation

- Documentation home: https://pandora-isomemo.github.io/PEITHO/
- Workflow quickstart (load, run, import/export): https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_basics.html
- Workflow execution and recovery (errors, resume): https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_execution_recovery.html
- Workflow authoring and editing (files, tags, update APIs): https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_authoring_editing.html

## Quick Example

This README provides a minimal starting point. Detailed workflow guidance is split across the workflow vignettes listed below.

### Local Installation

```R
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

### Run the built-in example workflow

Use the bundled example workflow to quickly verify your setup:

```R
# Load workflow from package example files
my_wf <- new_workflow(workflow_file_paths = workflow_file_paths(path = ""))

# Run a subset of steps
my_run <- run(my_wf, from = 1, to = 3)

# Inspect result size
length(my_run$state$last_result)
```

For custom-folder workflows, import/export workflows, and result inspection patterns, see the quickstart vignette in the Documentation section above.

## Choose a Workflow Guide

- Quickstart: [Workflow Quickstart in PEITHO](https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_basics.html)
- Execution and recovery: [Workflow Execution in PEITHO](https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_execution_recovery.html)
- Authoring and editing: [Workflow Authoring in PEITHO](https://pandora-isomemo.github.io/PEITHO/articles/peitho_workflow_authoring_editing.html)

## Notes for developers

### Documentation Updates

When adding information to help pages, docstrings, or vignettes, please update documentation locally as follows. The documentation of
the main branch is built automatically via GitHub Actions. Run these commands before opening a PR with doc or vignette changes.

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
