stepsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
        align = "right",
        tags$h3(textOutput(ns("dynamic_title")))
      ),
      column(6,
        style = "margin-top: 0.75em;",
        textInput(ns("step_name"), label = NULL, placeholder = "Name the workflow step (optional)", width = "100%")
      )
    ),
    tags$hr(),
    radioButtons(
      ns("source"),
      "Select external input source",
      choices = c("None", "Pandora Platform", "File", "URL"),
      selected = "None",
      inline = TRUE
    ),
    checkboxInput(
      ns("include_last"),
      "Also include last response as context",
      value = TRUE
    ),
    radioButtons(
      ns("operation"),
      "Select operation to apply (Availability will depend on 'input source')",
      choices = c(
        "chat (llm)" = "chat",
        "rag query (ragnar + llm)" = "rag_query",
        "rag summarize (ragnar)" = "rag_summarize",
        "fetch" = "fetch",
        "aggregate" = "aggregate",
        "text to list" = "text_to_list",
        "list to text" = "list_to_text",
        "other operations ..."
      ),
      selected = "chat",
      inline = TRUE
    ),
    tags$hr(),
    conditionalPanel(
      condition = "input.operation == 'chat'",
      ns = ns,
      llmModule::llm_generate_prompt_ui(
        ns("llm_prompt"),
        prompt_beginning = "Please follow the instructions below carefully.",
        prompt_placeholder = "... your natural language instructions ...",
        theme = "cobalt"
      )
    ),
    conditionalPanel(
      condition = "input.operation == 'rag_query' || input.operation == 'rag_summarize'",
      ns = ns,
      tags$div(
        "RAG functionality will be implemented soon..."
      ),
      tags$hr(),
    ),
    conditionalPanel(
      condition = "input.operation == 'fetch'",
      ns = ns,
      selectInput(
        ns("css_selector"),
        "Select CSS selector",
        choices = c("h1", "h2", "h3", "p", "li"),
        selected = c("h1", "h2", "h3", "p", "li"),
        multiple = TRUE
      ),
      tags$hr(),
    ),
    conditionalPanel(
      condition = "input.operation == 'aggregate'",
      ns = ns,
      tags$div(
        "Aggregation functionality will be implemented soon..."
      ),
      tags$hr(),
    ),
    verbatimTextOutput(ns("text_result"), placeholder = TRUE),
    tags$hr(),
    fluidRow(
      column(12, align = "center",
        actionButton(ns("prev_step"), "Previous Step"),
        actionButton(ns("next_step"), "Next Step")
      )
    )
  )
}

stepsServer <- function(
  id,
  current_step,
  steps,
  update_step_name,
  update_step_source_external,
  update_step_include_last
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Dynamic title showing current step ID
      output$dynamic_title <- renderText({
        step_id <- current_step()
        sprintf("Workflow step %s: ", step_id)
      })

      # Sync *from* metadata → UI for name, source, include_last
      observeEvent(
        list(current_step(), steps()),
        ignoreInit = TRUE,
        {
          step_id <- current_step()
          df <- steps()
          idx <- which(df$id == step_id)
          if (length(idx) != 1) return()

          # Name
          current_name <- df$name[idx]
          if (!identical(input$step_name, current_name)) {
            updateTextInput(session, "step_name", value = current_name)
          }

          # Source external
          current_source <- df$source_external[idx]
          if (!identical(input$source, current_source)) {
            updateRadioButtons(session, "source", selected = current_source)
          }

          # Include last
          current_include_last <- df$include_last[idx]
          if (!identical(input$include_last, current_include_last)) {
            updateCheckboxInput(session, "include_last", value = current_include_last)
          }
        }
      )

      # Sync *from* UI → metadata
      observeEvent(
        input$step_name,
        ignoreInit = TRUE,
        {
          step_id <- current_step()
          update_step_name(step_id, input$step_name)
        }
      )

      # Placeholder text result output
      output$text_result <- renderText({
        paste(
          "This is some placeholder text for step", current_step(), ".",
          "\nWe might display results here."
        )
      })

      # Helper: are we on the first/last step?
      is_first_step <- reactive({
        step_id <- current_step()
        if (is.null(step_id) || length(step_id) != 1) return(FALSE)
        min(steps()$id) == step_id
      })
      is_last_step <- reactive({
        df <- steps()
        step_id <- current_step()
        if (is.null(step_id) || length(step_id) != 1) return(FALSE)
        if (nrow(df) == 0) return(TRUE)
        step_id == max(df$id)
      })

      # Update the button label whenever step or steps change
      observeEvent(
        list(current_step(), steps()),
        {
          # Disable prev_step on the first step
          if (is_first_step()) {
            shinyjs::disable(ns("prev_step"), asis = TRUE)
          } else {
            shinyjs::enable(ns("prev_step"), asis = TRUE)
          }

          # Update next_step label
          if (is_last_step()) {
            updateActionButton(session, "next_step", label = "Add Step")
          } else {
            updateActionButton(session, "next_step", label = "Next Step")
          }
        },
        ignoreInit = FALSE
      )

      # Navigation buttons currently only show notifications
      # Later you can hook them into your step management logic
      observeEvent(input$prev_step, {
        showNotification("Go to previous step (to be implemented).")
      })

      observeEvent(input$next_step, {
        if (is_last_step()) {
          # Add a new step (you’ll need a function from the sidebar for this)
          showNotification("Add new step (to be implemented).")
        } else {
          # Go to the next step (also ideally a sidebar helper)
          showNotification("Go to next step (to be implemented).")
        }

        # later:
          # if (is_last_step()) {
          #   add_step() # e.g. via sidebar function
          # } else {
          #   goto_next_step()  # e.g. goto_step(current_step() + 1)
          # }
      })
    }
  )
}