shinyServer(function(input, output, session) {
  wf <- reactiveVal(NULL)
  wf_run <- reactiveVal(NULL)

  imported_wf <- importServer(
    id = "import_wf",
    ckanFileTypes = config()[["modelFileTypes"]],
    ignoreWarnings = TRUE,
    defaultSource = config()[["defaultSource"]],
    importType = "zip",
    fileExtension = config()[["fileExtension"]],
    options = importOptions(rPackageName = config()[["rPackageName"]])
  )

  observe({
    PEITHO:::logDebug("%s: Observing 'imported_wf'", session$ns("imported_wf"))
    req(imported_wf(), isTRUE(length(imported_wf()) > 0))

    wf_name <- tools::file_path_sans_ext(names(imported_wf())[1])

    # unzip the file to a temporary directory and get the paths of the workflow files
    temp_dir <- tempdir()
    unzip(imported_wf()[[1]], exdir = temp_dir)

    # list files in the temp directory to find workflow files
    list.files(temp_dir, recursive = TRUE)
    wf_file_paths <- workflow_file_paths(path = temp_dir)
    wv_value <- new_workflow(
      name = wf_name,
      workflow_file_paths = wf_file_paths,
      use_peitho_folder = TRUE
    )
    wf(wv_value)
    wf_run(NULL)
  }) |>
    bindEvent(imported_wf())

  observeEvent(input$example, {
    example_wf <- new_workflow(name = "example_workflow")
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
    })
    wf_run(wf_run_val)
  })

  # enable and disable the download button based on whether a workflow is loaded
  observe({
    if (is.null(wf())) {
      shinyjs::disable("download", asis = TRUE)
    } else {
      shinyjs::enable("download", asis = TRUE)
    }
  })

  output$download <- downloadHandler(
    filename = function() {
      "my_workflow.peitho"
    },
    content = function(file) {
      withProgress({
        PEITHO:::logDebug("%s: Entering 'download'")
        save_as_zip(wf(), file = file)
      },
      value = 0.8,
      message = "Downloading ...")

      removeModal()
    }
  )

  output$wf_table <- renderTable({
    wf_val <- wf()
    if (is.null(wf_val)) return(NULL)
    as.data.frame(wf_val)
  }, rownames = TRUE)

  output$inputs_table <- renderTable({
    wfr <- wf_run()
    if (is.null(wfr)) return(NULL)
    inputs <- extract_inputs(wfr)
    if (is.null(inputs) || length(inputs) == 0) return(NULL)
    # Convert named list to data.frame for display
    data.frame(
      name = names(inputs),
      value = unlist(inputs, use.names = FALSE),
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)

  output$results_table <- renderTable({
    wfr <- wf_run()
    if (is.null(wfr)) return(NULL)
    as.data.frame(wfr)
  }, rownames = TRUE)
})
