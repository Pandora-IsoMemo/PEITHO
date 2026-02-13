library(shiny)
library(shinyjs)
library(DataTools)
library(shinythemes)
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
          downloadButton("download", "Download")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Sequence", tableOutput("wf_table")),
            tabPanel("Inputs", tableOutput("inputs_table")),
            tabPanel("Results", tableOutput("results_table"))
          )
        ),
      )
    )
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
)
