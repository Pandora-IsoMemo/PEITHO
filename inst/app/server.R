shinyServer(function(input, output, session) {
  example_wf <- reactiveVal(NULL)
  wf_run <- reactiveVal(NULL)

  observeEvent(input$example, {
    wf <- new_workflow()
    example_wf(wf)
    wf_run(NULL)
  })

  observeEvent(input$run, {
    wf <- example_wf()
    if (is.null(wf)) return()

    wf_run_val <- NULL
    withProgress(message = "Running workflow...", value = 0, {
      wf_run_val <- run(
        wf,
        from = 1,
        to = length(wf$steps)
      )
    })
    wf_run(wf_run_val)
    output$progress_ui <- renderUI({ NULL })
  })

  # Custom message handler for updating progress bar
  session$onFlushed(function() {
    session$sendCustomMessage("updateProgressBar", list(id = "wf_progress", value = 0))
  })

  output$wf_table <- renderTable({
    wf <- example_wf()
    if (is.null(wf)) return(NULL)
    as.data.frame(wf)
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
