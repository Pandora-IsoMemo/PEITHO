shinyServer(function(input, output, session) {
  wf <- reactiveVal(NULL)
  wf_run <- reactiveVal(NULL)

  imported_wf <- DataTools::importServer(
    id = "import_wf",
    ckanFileTypes = config()[["fileExtension"]],
    ignoreWarnings = TRUE,
    defaultSource = config()[["defaultSource"]],
    importType = "zip",
    fileExtension = config()[["fileExtension"]],
    options = DataTools::importOptions(rPackageName = config()[["rPackageName"]])
  )

  active_temp_dir <- reactiveVal(NULL)

  observe({
    PEITHO:::logDebug("%s: Observing 'imported_wf'", session$ns("imported_wf"))
    req(imported_wf(), isTRUE(length(imported_wf()) > 0))

    # clean up the previous temp directory if it exists
    if (!is.null(active_temp_dir())) {
      PEITHO:::logDebug(
        "%s: Cleaning up previous temp directory: %s",
        session$ns("imported_wf"), active_temp_dir()
      )
      unlink(active_temp_dir(), recursive = TRUE, force = TRUE)
      active_temp_dir(NULL)
    }

    wf_name <- tools::file_path_sans_ext(names(imported_wf())[1])

    # unzip the file to a temporary directory and get the paths of the workflow files
    temp_dir <- tempfile(pattern = "workflow_")
    dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
    # do not use on.exit here because we want to keep the unzipped files around while the workflow
    # is loaded, and only delete them when a new workflow is loaded.
    active_temp_dir(temp_dir)
    unzip(imported_wf()[[1]], exdir = temp_dir) |>
      shinyTools::shinyTryCatch(
        errorTitle = "Error unzipping workflow",
        warningTitle = "Warning unzipping workflow"
      )

    wf_file_paths <- workflow_file_paths(path = temp_dir)
    wv_value <- new_workflow(
      name = wf_name,
      workflow_file_paths = wf_file_paths,
      use_peitho_folder = TRUE
    ) |>
      shinyTools::shinyTryCatch(
        errorTitle = "Error creating workflow",
        warningTitle = "Warning creating workflow"
      )
    wf(wv_value)
    wf_run(NULL)
  }) |>
    bindEvent(imported_wf())

  observeEvent(input$example, {
    example_wf <- new_workflow(name = "example_workflow") |>
      shinyTools::shinyTryCatch(
        errorTitle = "Error creating workflow",
        warningTitle = "Warning creating workflow"
      )
    wf(example_wf)
    wf_run(NULL)
  })

  observeEvent(input$run, {
    if (is.null(wf())) return()

    wf_run_val <- NULL
    shiny::withProgress(message = "Running workflow...", value = 0, {
      wf_run_val <- run(
        wf(),
        from = 1,
        to = length(wf()$steps)
      )
    }) |>
      shinyTools::shinyTryCatch(
        errorTitle = "Error running workflow",
        warningTitle = "Warning running workflow"
      )
    wf_run(wf_run_val)
  })

  # enable and disable the download button based on whether a workflow is loaded
  observe({
    if (is.null(wf())) {
      shinyjs::disable("download", asis = TRUE)
      # reset the filename input when no workflow is loaded
      updateTextInput(session, "userFileName", value = "")
    } else {
      shinyjs::enable("download", asis = TRUE)
      # update the filename input with the workflow name if it's not already set
      if (is.null(input$userFileName) || input$userFileName == "") {
        updateTextInput(session, "userFileName", value = wf()$name)
      }
    }
  })

  output$download <- downloadHandler(
    filename = function() {
      if (is.null(input$userFileName) || input$userFileName == "") {
        f_name <- paste0(wf()$name, ".peitho")
      } else {
        f_name <- paste0(input$userFileName, ".peitho")
      }
      f_name
    },
    content = function(file) {
      withProgress({
        PEITHO:::logDebug("%s: Entering 'download'", session$ns("download"))
        save_as_zip(wf(), file = file) |>
      shinyTools::shinyTryCatch(
        errorTitle = "Error downloading workflow",
        warningTitle = "Warning downloading workflow"
      )
      },
      value = 0.8,
      message = "Downloading ...")
    }
  )

  workflow_table_server("wf_table", wf)
  inputs_table_server("inputs_table", wf)
  results_table_server("results_table", wf_run)
})
