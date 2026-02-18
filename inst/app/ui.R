library(shiny)
library(PEITHO)

tagList(
  shinyjs::useShinyjs(),
  navbarPage(
    title = paste("PEITHO", packageVersion("PEITHO")),
    theme = shinythemes::shinytheme("flatly"),
    position = "fixed-top",
    collapsible = TRUE,
    id = "tab",
    tabPanel(
      title = "Tab",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          DataTools::importUI("import_wf", label = "Import"),
          actionButton("example", "Load Example"),
          tags$hr(),
          actionButton("run", "Run"),
          tags$hr(),
          tags$h4("Download Workflow"),
          textInput("userFileName", "File name (without extension)", value = "", width = "100%"),
          tags$br(),
          downloadButton("download", "Download")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Workflow", workflow_table_ui("wf_table")),
            tabPanel("Inputs", inputs_table_ui("inputs_table")),
            tabPanel("Results", results_table_ui("results_table"))
          )
        ),
      )
    )
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
)
