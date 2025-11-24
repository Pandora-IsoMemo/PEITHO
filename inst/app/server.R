shinyServer(function(input, output, session) {

  # Sidebar: manages steps list & current selection
  sidebar <- stepsSidebarServer("steps_sidebar")

  # Steps: configure currently selected step
  stepsServer(
    "steps",
    current_step     = sidebar$current_step,
    steps            = sidebar$steps,
    update_step_name = sidebar$update_step_name,
    update_step_source_external = sidebar$update_step_source_external,
    update_step_include_last  = sidebar$update_step_include_last
  )

  observeEvent(input$example, {
    showNotification("Create example workflow (to be implemented).")
  })
})
