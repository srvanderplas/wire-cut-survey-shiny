#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)
library(shinyjs)

consent_panel <- tabPanel(
  title = "Consent",
  id = "consent_tab",
  fluidRow(
    column(width = 10, offset = 1,
           includeMarkdown("consent.md")
    )
  ),
  fluidRow(
    column(width = 2, offset = 2,
           actionButton("consentbtn", "I Agree", icon = icon("screwdriver-wrench"), class = "btn-success")),

    column(width = 2, offset = 4,
           a("I Do Not Agree", icon = icon("xmark"), class = "btn btn-danger", href = "https://reddit.com/r/DIY"))
  ),
  fluidRow(
    column(
      width = 4, offset = 1,
      helpText("Click here if you agree; you will be automatically redirected to the instructions.")
    ),
    column(
      width = 4, offset = 2,
      helpText("Click here to decline to participate. You will be redirected to Reddit.")
    )
  )
)


instructions_panel <- tabPanel(
  title = "Instructions",
  id = "instruction_tab",
  tags$head(includeHTML("instructions-head.html")),
  fluidRow(
    column(width = 10, offset = 1, includeHTML("instructions.html"))
  )
)

data_panel <- tabPanel(
  title = "Data Entry",
  id = "data_tab"
)

completion_panel <- tabPanel(
  title = "Study Completion",
  id = "completion_tab"
)

# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "Wire Cutting Tools Survey",
  id = "tab",
  header = useShinyjs(),
  consent_panel,
  instructions_panel,
  data_panel,
  completion_panel
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # disable tabs Instructions, Data Entry
  shinyjs::disable(selector = '.navbar-nav a[data-value="Instructions"')
  shinyjs::disable(selector = '.navbar-nav a[data-value="Data Entry"')
  shinyjs::disable(selector = '.navbar-nav a[data-value="Study Completion"')

  tab_id <- c("Consent", "Instructions", "Data Entry", "Completion")

  consent <- reactiveValues(time = NA, btn = NA)

  observeEvent(input$consentbtn, {
    consent$time <- min(consent$time, Sys.time(), na.rm = T)
    consent$btn <- "Consent"

    shinyjs::enable(selector = '.navbar-nav a[data-value="Instructions"')

    updateNavbarPage(
      inputId = "tab",
      selected = "Instructions"
    )
  })

  observeEvent(input$declinebtn, {
      consent$time <- min(consent$time, Sys.time(), na.rm = T)
      consent$btn <- "Decline"
      shinyjs::runjs(paste0('window.location.href = "https://reddit.com/r/DIY/";'))
  })


}

# Run the application
shinyApp(ui = ui, server = server)
