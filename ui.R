library(shinycssloaders)
library(hodfr)
# https://www.statworx.com/en/content-hub/blog/dynamic-ui-elements-in-shiny/

# Placeholder for translation machinations
T <- function (s) s

branding_name <- 'ModelWizard'
if (file.exists('branding.R')) source('branding.R')

label <- function (...) htmltools::tag('label', ...)

timestep_choices = list(
    "Annual (1)" = 1,
    "Bi-annual (2)" = 2,
    "Quarterly (4)" = 4,
    "Monthly (12)" = 12)
names(timestep_choices) <- vapply(names(timestep_choices), T, character(1))

navbarPage(id = "nav", windowTitle = branding_name, lang = 'en',
  title = div(
    span(branding_name),
    a(icon("github", lib = "font-awesome"),
      href="https://github.com/gadget-framework/modelwizard",
      class="navbar-brand",
      style="position: absolute; top: 0; right: 0")),
  header = tag('link', list(href="styles.css", rel="stylesheet")),

  tabPanel(T("Load / save"),
      div(class="row",
          div(class="col-md-3",
              fileInput('loadCSV', T('Load spreadsheet'),
                  accept = c('text/csv', 'text/comma-separated-values', 'text/tab-separated-values', 'text/plain', '.csv', '.tsv')),
              div(style = "margin-top: -15px", span("...or"), actionLink("loadDemo", T("Load demo data")))),
          div(class="col-md-3",
              textInput('filename', NULL, label=T("Filename to save as"))),
          div(class="col-md-3",
              downloadButton("saveCSV", T("Save data to CSV"), style = "margin-top: 25px"))),
      h3('Data description'),
      ""),

  tabPanel(T("Specification"),
      h3(T("Area")),
      textInput('area_name', 'all', label=T("Area name")),
      hr(),

      h3(T("Time")),
      div(class="row",
          div(class="col-md-3", numericInput('time_year_min', T("Start year for model"), 1990, step = 1)),
          div(class="col-md-3", numericInput('time_year_max', T("End year for model"), 2020, step = 1)),
          ""),
      div(class="row",
          div(class="col-md-3", selectInput('time_steps', T("Steps within a year"), timestep_choices)),
          ""),
      hr(),

      h3("Stock"),
      uiOutput("stocks"),

      h3("Fleets"),
      uiOutput("fleets"),
      hr(),

      h3("Abundance indices"),
      uiOutput("abund_idx"),
      hr(),
      ""),

  tabPanel(T("Data"),
      uiOutput("fleets_survey_data"),
      ""),

  tabPanel(T("Parameters"),

      ""),

  footer = includeHTML("footer.html"))
