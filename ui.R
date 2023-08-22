library(shinycssloaders)
library(hodfr)
# https://www.statworx.com/en/content-hub/blog/dynamic-ui-elements-in-shiny/

# Placeholder for translation machinations
T <- function (s) s

branding_name <- 'ModelWizard'
branding_footer <- 'footer.html'
if (file.exists('branding.R')) source('branding.R', local = TRUE)

label <- function (...) htmltools::tag('label', ...)

timestep_choices = list(
    "Annual (1)" = 1,
    "Bi-annual (2)" = 2,
    "Quarterly (4)" = 4,
    "Monthly (12)" = 12)
names(timestep_choices) <- vapply(names(timestep_choices), T, character(1))

navbarPage(id = "nav_tabs", windowTitle = branding_name, lang = 'en',
  title = div(
    span(branding_name),
    a(icon("github", lib = "font-awesome"),
      href="https://github.com/gadget-framework/modelwizard",
      class="navbar-brand",
      style="position: absolute; top: 0; right: 0")),
  header = tag('link', list(href="styles.css", rel="stylesheet")),

  tabPanel(T("Specification"),
      div(class="row",
          div(class="col-md-3",
              fileInput('file_load', T('Load spreadsheet'),
                  accept = c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', '.xlsx')),
              div(style = "margin-top: -15px", span("...or"), actionLink("file_load_demo_act", T("Load demo data")))),
          div(class="col-md-3",
              textInput('file_name', 'modelwizard', label=T("Filename to save as"))),
          div(class="col-md-3",
              downloadButton("file_save_act", T("Save data to spreadsheet"), style = "margin-top: 25px"))),
      hr(),

      h3(T("Area")),
      textInput('area_1_name', 'all', label=T("Area name")),
      hr(),

      h3(T("Time")),
      div(class="row",
          div(class="col-md-3", numericInput('time_1_year_min', T("Start year for model"), 1990, step = 1)),
          div(class="col-md-3", numericInput('time_1_year_max', T("End year for model"), 2020, step = 1)),
          ""),
      div(class="row",
          div(class="col-md-3", selectInput('time_1_steps', T("Steps within a year"), timestep_choices)),
          ""),
      hr(),

      h3(T("Stock")),
      uiOutput("stocks"),

      h3(T("Fleets")),
      uiOutput("fleets"),
      hr(),

      h3(T("Abundance indices")),
      uiOutput("abund"),
      hr(),
      ""),

  tabPanel(T("Data"), value = 'data',
      uiOutput("fleets_data"),
      ""),

  tabPanel(T("Parameters"), value = 'parameters',
      h3(T("Model parameters")),
      verbatimTextOutput('parameters_error'),
      hodfr::hodfr(
          'params',
          fields = list(
              list(name="switch", title=T("Parameter name")),
              list(name="value", title=T("Initial value"), content="numeric"),
              list(name="optimise", title=T("Optimise Parameter?"), content="checkbox"),
              list(name="lower", title=T("Lower bound"), content="numeric"),
              list(name="upper", title=T("Upper bound"), content="numeric")),
          values = list(type = "bins"),
          orientation = 'horizontal', js_debug = FALSE),
      ""),

  tabPanel(T("Gadget3 script"), value = 'script_g3',
      p(
        T("If you haven't already"),
        downloadLink("file_save_g3_act", T("download your data as a spreadsheet")),
        T("then copy and paste the script below:"),
        ""),
      verbatimTextOutput('script_g3_text'),
      ""),

  tabPanel(T("SS3/r4ss script"), value = 'script_ss',
      p(
        T("If you haven't already"),
        downloadLink("file_save_ss_act", T("download your data as a spreadsheet")),
        T("then copy and paste the script below:"),
        ""),
      verbatimTextOutput('script_ss_text'),
      ""),

  footer = includeHTML(branding_footer))
