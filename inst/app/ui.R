library(shiny)
library(PEITHO)

tagList(
  useShinyjs(),
  navbarPage(
    title = paste("PEITHO", packageVersion("PEITHO")),
    theme = shinythemes::shinytheme("flatly"),
    position = "fixed-top",
    collapsible = TRUE,
    id = "tab",
    tabPanel(
      title = "RAG Workflow Builder",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          stepsSidebarUI("steps_sidebar"),
          tags$hr(),
          actionButton("example", "Create Example", width = "100%")
        ),
        mainPanel(
          width = 10,
          stepsUI("steps")
        ),
      )
    )
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
)
