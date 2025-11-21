stepsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(5, align = "left",
        tags$h2("RAG Workflow Builder")
      ),
      column(2, align = "center",
        titlePanel(textOutput(ns("dynamic_title")))
      ),
      column(5,
        style = "margin-top: 1em;",
        sliderInput(ns("step"), label = NULL, min = 1, max = 3, value = 3, width = "100%")
      )
    ),
    tags$hr(),
    radioButtons(
      ns("source"),
      "Select input source",
      choices = c("Last response", "Pandora Platform", "File", "URL"),
      selected = "Last response",
      inline = TRUE
    ),
    radioButtons(
      ns("function"),
      "Select function to apply (Availability will depend on 'input source')",
      choices = c(
        "chat",
        "fetch",
        "aggregate",
        "text to table",
        "table to text",
        "other functions ..."
      ),
      selected = "chat",
      inline = TRUE
    ),
    tags$hr(),
    conditionalPanel(
      condition = "input.function == 'chat'",
      ns = ns,
      llmModule::llm_generate_prompt_ui(
        ns("llm_prompt"),
        prompt_beginning = "Please follow the instructions below carefully.",
        prompt_placeholder = "... your natural language instructions ...",
        theme = "cobalt"
      )
    ),
    conditionalPanel(
      condition = "input.function == 'fetch'",
      ns = ns,
      selectInput(
        ns("css_selector"),
        "Select CSS selector",
        choices = c("h1", "h2", "h3", "p", "li"),
        selected = c("h1", "h2", "h3", "p", "li"),
        multiple = TRUE
      ),
    ),
    conditionalPanel(
      condition = "input.function == 'aggregate'",
      ns = ns,
      tags$div(
        "Aggregation functionality will be implemented soon..."
      )
    ),
    tags$hr(),
    verbatimTextOutput(ns("text_result"), placeholder = TRUE),
    tags$style(HTML("
      #text_result {
        height: 400px;
        overflow-y: auto;
        font-size: 1.2em;
        background-color: #f8f8f8;
        border: 1px solid #ccc;
        padding: 16px;
      }
    ")),
    tags$hr(),
    fluidRow(
      column(12, align = "center",
        actionButton(ns("prev_step"), "Previous Step"),
        actionButton(ns("next_step"), "Next Step"),
        actionButton(ns("export_steps"), "Export Workflow")
      )
    )
  )
}

stepsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$dynamic_title <- renderText({
        paste("Step", input$step)
      })
      output$text_result <- renderText({
        "This is some placeholder text.\nWe might display results here."
      })
    }
  )
}