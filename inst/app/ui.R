library(shiny)
library(PEITHO)

tagList(
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
          actionButton("example", "Create Example"),
          br(),
          br(),
          actionButton("run", "Run Example"),
          br(),
          br(),
          uiOutput("progress_ui")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("sequence", tableOutput("wf_table")),
            tabPanel("inputs", tableOutput("inputs_table")),
            tabPanel("results", tableOutput("results_table"))
          )
        ),
      )
    )
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
)
