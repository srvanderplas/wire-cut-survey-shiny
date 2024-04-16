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
library(xml2)
library(htmltools)

## Process HTML files and make into txt files
html_deps <- c("www/prep.html", "www/cutting-surface-instructions.html", "www/prying-surface-instructions.html")

header_info <- function(base, head) {
  # file.copy(paste0("www/", base, '*'), "www/", overwrite = T)
  header <- purrr::map(xml_children(head), ~tag(xml_name(.), xml_attrs(.))) %>% as.tags() %>% singleton()
  tags$head(header)
}

html_split <- function(fname) {

  base <- basename(fname) %>% tools::file_path_sans_ext() %>% paste0(., "_files/libs/")
  doc_txt <- readLines(fname, warn = F) %>% stringr::str_remove_all(base) %>% paste(collapse = "")
  doc <- read_html(doc_txt) %>% xml_children()

  # Need to delete any nodes where src or href matches "bootstrap/"

  list(
    header_info(base, doc[1]),
    HTML(as.character(doc[2]))
    )
}


consent_panel <- tabPanel(
  title = "Consent",
  id = "consent_tab",
  useShinyjs(),
  fluidRow(
    column(width = 2),
    column(width = 8,
           includeMarkdown("consent.md")),
    column(width = 2)
  ),
  fluidRow(
    column(width = 2), # margin
    column(width = 8,
           fluidRow(
             column(
               width = 2, offset = 2,
               actionButton("consentbtn", "I Agree", icon = icon("screwdriver-wrench"), class = "btn-success")
             ),
             column(
               width = 2, offset = 4,
               a("I Do Not Agree", icon = icon("xmark"), class = "btn btn-danger", href = "https://reddit.com/r/DIY")
             )
           )),
    column(width = 2) # margin
  ),
  fluidRow(
    column(width = 2),
    column(
      width = 2, offset = 1,
      helpText("Click here if you agree; you will be automatically redirected to the instructions.")
    ),
    column(
      width = 2, offset = 2,
      helpText("Click here to decline to participate. You will be redirected to Reddit.")
    ),
    column(width = 2)
  )
)


instructions_panel <- tabPanel(
  title = "Instructions",
  id = "instruction_tab",
  fluidRow(
    column(width = 10, offset = 1, html_split("www/prep.html"))
  )
)

wirecut_panel <- tabPanel(
  title = "Wire Cutters",
  id = "wirecut_tab",
  fluidRow(
    column(width = 7, html_split("www/cutting-surface-instructions.html")),

    column(width = 5,
           rHandsontableOutput("wirecut"),
           actionButton("saveWirecutBtn", label = "Save Wire Cutter Data"))
  )
)

pry_panel <- tabPanel(
  title = "Prying Tools",
  id = "pry_tab",
  fluidRow(
    column(width = 7, html_split("www/prying-surface-instructions.html")),

    column(width = 5,
           rHandsontableOutput("pry"),
           actionButton("savePryBtn", label = "Save Prying Tools Data"))
  )
)

completion_panel <- tabPanel(
  title = "Study Completion",
  id = "completion_tab"
)

header <- list()#HTML(readLines("www/header.html"))


ui <- navbarPage(
  title = "DIY Tool Survey",
  id = "tab",
  header = header,
  consent_panel,
  instructions_panel,
  wirecut_panel,
  pry_panel,
  completion_panel
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # disable tabs Instructions, Data Entry
  shinyjs::disable(selector = '.navbar-nav a[data-value="Instructions"]')
  shinyjs::disable(selector = '.navbar-nav a[data-value="Wire Cutters"]')
  shinyjs::disable(selector = '.navbar-nav a[data-value="Prying Tools"]')
  shinyjs::disable(selector = '.navbar-nav a[data-value="Study Completion"]')

  tab_id <- c("Consent", "Instructions", "Wire Cutters", "Prying Tools", "Completion")

  consent <- reactiveValues(time = NA, btn = NA)


  observe({
    # remove button and isolate to update file automatically
    # after each table change
    input$saveWirecutBtn
    wirecut = isolate(input$wirecut)
    if (!is.null(wirecut)) {
      # write.csv(hot_to_r(input$hot), fname) # Save data to table
      # print(fname)
    }
  })

  output$wirecut <- renderRHandsontable({
    if (!is.null(input$wirecut)) {
      DF = hot_to_r(input$wirecut)
    } else {
      DF = data.frame("Tool Description" = c("Example: Scissors", rep("", 10)),
                      "Blade Length (cm)" = c(8, rep(0, 10)),
                      "Num Cutting Surfaces" = c(4, rep(0, 10)),
                      "Count" = NA_integer_, check.names = F)
    }

    rhandsontable(DF, colHeaders = c("Tool Description", "Blade Length (cm)", "Num Cutting Surfaces", "Count"),
                  width = "100%") %>%
      hot_col("Num Cutting Surfaces", format = "0") %>%
      hot_col("Count", format = "0") %>%
      hot_row(1, readOnly = T)
  })



  observe({
    # remove button and isolate to update file automatically
    # after each table change
    input$savePryBtn
    pry = isolate(input$pry)
    if (!is.null(pry)) {
      # write.csv(hot_to_r(input$hot), fname) # Save data to table
      # print(fname)
    }
  })

  output$pry <- renderRHandsontable({
    if (!is.null(input$pry)) {
      DF = hot_to_r(input$pry)
    } else {
      DF = data.frame("Tool Description" = c("Example: Hammer", rep("", 10)),
                      "Surface Length (cm)" = c(3, rep(0, 10)),
                      "Claw Length (cm)" = c(0.7, rep(0, 10)), check.names = F)
    }

    rhandsontable(DF)

    rhandsontable(DF, colHeaders = c("Tool Description", "Surface Length (cm)", "Claw Length (cm)"),
                  width = "100%") %>%
      hot_row(1, readOnly = T)
  })


  observeEvent(input$consentbtn, {
    consent$time <- min(consent$time, Sys.time(), na.rm = T)
    consent$btn <- "Consent"

    shinyjs::enable(selector = '.navbar-nav a[data-value="Instructions"]')
    shinyjs::enable(selector = '.navbar-nav a[data-value="Wire Cutters"]')
    shinyjs::enable(selector = '.navbar-nav a[data-value="Prying Tools"]')

    updateNavbarPage(session, "tab", tab_id[2])

  })

  observeEvent(input$declinebtn, {
      consent$time <- min(consent$time, Sys.time(), na.rm = T)
      consent$btn <- "Decline"
      shinyjs::runjs(paste0('window.location.href = "https://reddit.com/r/DIY/";'))
  })


}

# Run the application
shinyApp(ui = ui, server = server)
