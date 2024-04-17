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
library(stringr)
library(rvest)
library(purrr)
library(bslib)

## Process HTML files and make into txt files
html_deps <- c("www/prep.html", "www/cutting-surface-instructions.html", "www/prying-surface-instructions.html")

header_info <- function(base, head) {
  header <- purrr::map(xml_children(head), ~tag(xml_name(.), xml_attrs(.))) %>%
    as.tags() %>%
    singleton()
  tags$head(header)
}

html_split <- function(fname) {

  base <- basename(fname) %>% tools::file_path_sans_ext() %>% paste0(., "_files/libs/")
  doc_txt <- readLines(fname, warn = F) %>% stringr::str_remove_all(base) %>% paste(collapse = "")
  doc <- read_html(doc_txt) %>% xml_children()

  # Need to delete any nodes where src or href matches "bootstrap/"
  rvest::html_elements(doc, "link[href*='bootstrap/']") %>% purrr::walk(xml_remove)
  rvest::html_elements(doc, "script[src*='bootstrap/']") %>% purrr::walk(xml_remove)


  list(
    header_info(base, doc[1]),
    HTML(as.character(doc[2]))
    )
}


make_iframe <- function(file) {
  tags$iframe(frameborder="0",
              "allowfullscreen",
              width="100vw",
              height="100vh",
              style="width:100%;height:100vh;overflow:hidden;",
              src=file)
}

consent_panel <- nav_panel(
  title = "Consent",
  id = "consent_tab",
  useShinyjs(),
    layout_columns(
      col_widths = c(12, 6, 6),
      card(make_iframe("consent.html")),
      card(actionButton("consentbtn", "I Agree", icon = icon("screwdriver-wrench"), class = "btn-success"),
           card_footer(helpText("Click here if you agree; you will be automatically redirected to the instructions."))),
      card(a("I Do Not Agree", icon = icon("xmark"), class = "btn btn-danger", href = "https://reddit.com/r/DIY"),
           card_footer(helpText("Click here to decline to participate. You will be redirected to Reddit."))))
)


bkgd_panel <- nav_panel(
  title = "Background",
  id = "bkgd_tab",
  layout_columns(col_widths = c(10, 2),
                 card(make_iframe("bkgd.html")),
                 card(actionButton("bkgd_next", "Next Page")))
)

inst_panel <- nav_panel(
  title = "Instructions",
  id = "inst_tab",
  layout_columns(col_widths = c(10, 2),
                 card(make_iframe("prep.html")),
                 card(actionButton("inst_next", "Next Page")))
)

wirecut_panel <- nav_panel(
  title = "Wire Cutters",
  id = "wirecut_tab",
  layout_columns(col_widths = c(7, 5),
                 card(make_iframe("cutting-surface-instructions.html")),
                 card(h2("Data Input"),
                      rHandsontableOutput("wirecut"),
                      actionButton("wire_next", "Next Page")))
)


pry_panel <- nav_panel(
  title = "Prying Tools",
  id = "pry_tab",
  layout_columns(col_widths = c(7, 5),
                 card(make_iframe("prying-surface-instructions.html")),
                 card(h2("Data Input"),
                      rHandsontableOutput("pry"),
                      actionButton("pry_next", "Next Page")))
)


completion_panel <- nav_panel(
  title = "Demographics",
  id = "completion_tab",
  layout_columns(
    col_widths = c(4, 4, 4),
    card(card_header("Demographics"),
         card_body(
           selectInput("age", "Age", choices = c("Under 19", "19-24", "25-34", "35-44", "45-54", "55-64", "64+"), selectize = T, multiple = F, width = "100%"),
           selectInput("gender", "Gender Identity", choices = c("Male", "Female", "Nonbinary", "Other"), width = "100%"),
           checkboxInput("hispanic", "Are you of Hispanic, Latino/a/x, or Spanish origin?", width = "100%"),
           checkboxGroupInput("race", "Race and Ethnicity", choices = c("White", "Black or African", "American Indian/Alaska Native", "Middle East/North Africa", "Asian", "Pacific Islander"), width = "100%")
         )),
    card(card_header("DIY Profile"),
         card_body(
           p("We are interested in understanding how you use your tools. Please select any of the following activities that you use your tools to participate in. Feel free to elaborate in the comment box."),
           selectInput("hobbies", "DIY Activities", choices = c("Automotive", "Woodworking", "3D printing", "Electronics", "Home Improvement", "Jewlery making", "Fiber arts (crochet, knitting, tufting, weaving)", "Metal Working", "Sewing", "Scrapbooking", "Cooking", "Outdoor"), selectize = T, multiple = T, width = "100%"),
           textAreaInput("hobbies_txt", label = "Is there anything else you'd like to say about your tool collection and hobbies? (Please keep this SFW)", width = "100%", height = "200px")
         )),
    card(textAreaInput("comments", label = "Other Comments", placeholder = "Please provide any other comments you have here.", width = "100%", height = "200px"),
         card_footer(actionButton("comment_submit", "Submit Demographics", width = "100%")))
  )
)


header <- tags$head(tags$style(href="quarto-figure.css"))


ui <- page_navbar(
  title = "DIY Tool Survey",
  id = "tab",
  header = header,
  bkgd_panel,
  consent_panel,
  inst_panel,
  wirecut_panel,
  pry_panel,
  completion_panel
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  tab_id <- c("Background", "Consent", "Instructions",
              "Wire Cutters", "Prying Tools", "Demographics")
  tab_selectors <- sprintf(".navbar-nav a[data-value='%s']", tab_id)
  purrr::walk(tab_selectors[3:6], ~shinyjs::disable(selector = .))

  consent <- reactiveValues(time = NA, btn = NA)


  observe({ if(input$bkgd_next > 0) { updateNavbarPage(session, "tab", tab_id[2]) }})
  observeEvent(input$consentbtn, {
    consent$time <- min(consent$time, Sys.time(), na.rm = T)
    consent$btn <- "Consent"
    purrr::walk(tab_selectors[3:6], ~shinyjs::enable(selector = .))
    updateNavbarPage(session, "tab", tab_id[3])
  })
  observe({ if(input$inst_next > 0) { updateNavbarPage(session, "tab", tab_id[4]) }})

  observe({
    if(input$wire_next > 0) {
      wirecut = isolate(input$wirecut)
      if (!is.null(wirecut)) {
        # write.csv(hot_to_r(input$hot), fname) # Save data to table
        # print(fname)
      }
      updateNavbarPage(session, "tab", tab_id[5])
    }
  })

  observe({
    if(input$pry_next > 0) {
      input$pry_next
      pry = isolate(input$pry)
      if (!is.null(pry)) {
        # write.csv(hot_to_r(input$hot), fname) # Save data to table
        # print(fname)
      }
      updateNavbarPage(session, "tab", tab_id[6])
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

  observeEvent(input$declinebtn, {
      consent$time <- min(consent$time, Sys.time(), na.rm = T)
      consent$btn <- "Decline"
      shinyjs::runjs(paste0('window.location.href = "https://reddit.com/r/DIY/";'))
  })


}

# Run the application
shinyApp(ui = ui, server = server)
