shinyServer(function(input, output, session) {
  wf <- reactiveVal(NULL)
  wf_run <- reactiveVal(NULL)

  observeEvent(input$example, {
    example_wf <- new_workflow()
    wf(example_wf)
    wf_run(NULL)
  })

  observeEvent(input$run, {
    if (is.null(wf())) return()

    wf_run_val <- NULL
    withProgress(message = "Running workflow...", value = 0, {
      wf_run_val <- run(
        wf(),
        from = 1,
        to = length(wf()$steps)
      )
    })
    wf_run(wf_run_val)
  })

  # observe({
  #   zipfile_path <- "./my_workflow.peitho"
  #   save_as_zip(wf(), file = zipfile_path)
  # }) %>%
  #   bindEvent(input$download_wf)

  output$wf_table <- renderTable({
    wf <- wf()
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
