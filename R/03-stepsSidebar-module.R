stepsSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h4("Workflow steps"),
    uiOutput(ns("steps_list")),
    tags$hr(),
    actionButton(ns("export_steps"), "Export Workflow", width = "100%")
  )
}

stepsSidebarServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Placeholder steps: id, name, type, source
      steps_rv <- reactiveVal(
        data.frame(
          id     = 1:3,
          name   = c("Initial chat", "Fetch URLs", "Aggregate results"),
          type   = c("chat", "fetch", "aggregate"),
          source_external = c("None", "URL", "None"),
          include_last   = c(TRUE, TRUE, FALSE),
          stringsAsFactors = FALSE
        )
      )

      # Render as radioButtons for now (simple & easy to click)
      output$steps_list <- renderUI({
        steps <- steps_rv()

        src_label <- vapply(seq_len(nrow(steps)), function(i) {
          src <- steps$source_external[i]
          incl <- steps$include_last[i]

          if (src == "None" && incl) {
            "last only"
          } else if (src == "None" && !incl) {
            "none"
          } else if (src != "None" && incl) {
            paste0(src, " + last")
          } else { # src != "None" && !incl
            src
          }
        }, character(1))

        # Label: "Step 1: Initial chat [chat | src: last only]"
        labels <- paste0(
            "Step ", steps$id, ": ",
            steps$name,
            " ['", steps$type, "' | src: ", src_label, "]"
          )
        choices <- steps$id
        names(choices) <- labels

        radioButtons(
          inputId = ns("step_select"),
          label   = "",
          choices = choices,
          selected = steps$id[3]
        )
      })

      # "Add step" is just a placeholder for now
      observeEvent(input$add_step, {
        showNotification("Add step: not implemented yet (placeholder).")
        # Later: append a new row to steps_rv()
      })

      # Helper to update step name from outside (e.g. stepsServer)
      update_step_name <- function(step_id, new_name) {
        df <- steps_rv()
        idx <- which(df$id == step_id)
        if (length(idx) == 1) {
        df$name[idx] <- new_name
        steps_rv(df)
        }
      }

      update_step_source_external <- function(step_id, source_external) {
        df <- steps_rv()
        idx <- which(df$id == step_id)
        if (length(idx) == 1) {
          df$source_external[idx] <- source_external
          steps_rv(df)
        }
      }

      update_step_include_last <- function(step_id, include_last) {
        df <- steps_rv()
        idx <- which(df$id == step_id)
        if (length(idx) == 1) {
          df$include_last[idx] <- include_last
          steps_rv(df)
        }
      }

      # Return the currently selected step id + the steps list
      list(
        current_step = reactive(input$step_select),
        steps        = steps_rv,
        update_step_name = update_step_name,
        update_step_source_external = update_step_source_external,
        update_step_include_last  = update_step_include_last
      )
    }
  )
}
