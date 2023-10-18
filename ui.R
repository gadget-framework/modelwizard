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
  title =  tagList(branding_title,
    a(icon("github", lib = "font-awesome"),
      href="https://github.com/gadget-framework/modelwizard",
      class="navbar-brand",
      style="position: absolute; top: 0; right: 0")),
  header = tagList(
    # https://stackoverflow.com/a/58585251
    tags$script("Shiny.addCustomMessageHandler('select_textarea', function(inputId) {
      $('#' + inputId).prop('spellcheck', false);  // It's code, no need to spellcheck
      $('#' + inputId).focus();
      // Wait for content to populate before selecting
      window.setTimeout(function () {
        $('#' + inputId).select();
      }, 100);
    });"),
    tag('link', list(href="styles.css", rel="stylesheet"))),

  tabPanel(T("Load / save"),
    div(class="container",
      div(class="row",
          div(class="col-md-3",
              fileInput('file_load', T('Load spreadsheet'),
                  accept = c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', '.xlsx'))),
          div(class="col-md-3",
              div(style = "margin-top: 30px", span("...or"), actionLink("file_load_demo_act", T("Load demo data"))))),
      div(class="row",
          div(class="col-md-3",
              textInput('file_name', 'modelwizard', label=T("Filename to save as"))),
          div(class="col-md-3",
              downloadButton("file_save_act", T("Save data to spreadsheet"), style = "margin-top: 25px"))),
      "")),

  tabPanel(T("Specification"),
    div(class="container",
      h3(T("Area")),
      div(class="panel panel-default panel-body",
        textInput('area_1_name', 'all', label=T("Area name")),
        p(class="help-block", T("An identifier to use to describe the single-area within your model. Letters, numbers and underscore are allowed.")),
        ""),
      hr(),

      h3(T("Time")),
      div(class="panel panel-default panel-body",
        div(class="row",
          div(class="col-md-3", numericInput('time_1_year_min', T("Start year for model"), 1990, step = 1)),
          div(class="col-md-3", numericInput('time_1_year_max', T("End year for model"), 2020, step = 1)),
          ""),
        div(class="row",
          div(class="col-md-3", selectInput('time_1_steps', T("Steps within a year"), timestep_choices)),
          ""),
        p(class="help-block", T("The default years your model will run from/until.")),
        ""),
      hr(),

      h3(T("Stock")),
      uiOutput("stocks"),
      hr(),

      h3(T("Commercial & survey fleets")),
      uiOutput("fleets"),
      hr(),

      h3(T("Abundance indices")),
      uiOutput("abund"),
      hr(),
      "")),

  tabPanel(T("Data"), value = 'data',
      p(class="help-block", T("To start from scratch, first press 'Clear and prepopulate values'. Then either fill in the data here, or save as spreadsheet on the previous tab, fill in the data and re-load.")),
      p(class="help-block", T("The available tables are based on the options selected in the 'Specification' tab, if a table is missing go back and change options there.")),
      uiOutput("all_data"),
      br(),
      ""),

  tabPanel(T("Gadget3 script"), value = 'script_g3',
    div(class="container",
      p(
        T("If you haven't already"),
        downloadLink("file_save_g3_act", T("download your data as a spreadsheet")),
        T("then copy and paste the script below:"),
        ""),
      textAreaInput('script_g3_text', value = T("Loading..."), label = T("Code"), width = "100%", height = "80rem"),
      "")),

  tabPanel(T("SS3/r4ss script"), value = 'script_ss',
    div(class="container",
      p(
        T("If you haven't already"),
        downloadLink("file_save_ss_act", T("download your data as a spreadsheet")),
        T("then copy and paste the script below:"),
        ""),
      textAreaInput('script_ss_text', value = T("Loading..."), label = T("Code"), width = "100%", height = "80rem"),
      "")),

  footer = includeHTML(branding_footer))
