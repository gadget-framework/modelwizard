# Placeholder for translation machinations
T <- function (s) s

# Populate a uiOutput section with an array of fields
reactiveSections <- function (input, val_name, ui_func, default_count = 0, button_add = TRUE, button_remove = TRUE) {
    rv <- reactiveVal(default_count, paste0(val_name, ' count'))
    observeEvent(input[[paste0(val_name, '_add')]], { rv(rv() + 1) })
    observeEvent(input[[paste0(val_name, '_remove')]], { rv(max(0, rv() - 1)) })
    return(renderUI(do.call(tagList, c(
        lapply(seq_len(rv()), function (i) {
            genId <- function (s) sprintf('%s_%d_%s', val_name, i, s)
            return(ui_func(genId))
        }),
        list(
            if (button_add) actionButton(paste0(val_name, '_add'), T("Add new")) else "",
            if (button_remove) actionButton(paste0(val_name, '_remove'), T("Remove")) else "",
            "")))))
}

server <- function(input, output, session) {
    timestepChoices <- reactive(structure(
        as.list(seq(0, input$time_steps)),
        names = c(T("Every timestep"), seq_len(input$time_steps))))

    output$stocks <- reactiveSections(input, 'stock', function (genId) tagList(
        textInput(genId('name'), NULL, label=T("Identifier")),
        div(class="row",
            div(class="col-md-3", numericInput(genId('lg_min'), T("Minimum length group"), 0)),
            div(class="col-md-3", numericInput(genId('lg_max'), T("Maximum length group"), 10)),
            div(class="col-md-3", numericInput(genId('lg_size'), T("Length group size"), 10)),
            ""),
        div(class="row",
            div(class="col-md-3", numericInput(genId('age_min'), T("Minimum age"), 0)),
            div(class="col-md-3", numericInput(genId('age_max'), T("Maximum age"), 10)),
            ""),
        numericInput(genId('max_lgg'), NULL, label=T("Maximum growth in a single step, in lengthgroups")),
        selectInput(genId('renewal_step'), T("Renewal at step"), timestepChoices(), selected = isolate(input[[genId('step')]])),
        hr()), default_count = 1, button_add = FALSE, button_remove = FALSE)

    output$fleets_comm <- reactiveSections(input, 'fleet_comm', function (genId) tagList(
        textInput(genId('name'), isolate(input[[genId('name')]]), label=T("Fleet identifier")),
        selectInput(genId('step'), T("Active at step"), timestepChoices(), selected = isolate(input[[genId('step')]])),
        selectInput(genId('catchability'), T("Quota in"), structure(
            c('weight', 'number'),
            names = c(T('Tonnes'), T('Number of individuals'))), selected = isolate(input[[genId('catchability')]])),
        selectInput(genId('landings'), T("Landings observations in"), structure(
            c('weight', 'number'),
            names = c(T('Tonnes'), T('Number of individuals'))), selected = isolate(input[[genId('landings')]])),
        hr()))

    output$fleets_survey <- reactiveSections(input, 'fleet_survey', function (genId) tagList(
        textInput(genId('name'), isolate(input[[genId('name')]]), label=T("Fleet identifier")),
        selectInput(genId('step'), T("Active at step"), timestepChoices(), selected = isolate(input[[genId('step')]])),
        hr()))

    output$abundance <- reactiveSections(input, 'abund_idx', function (genId) tagList(
        textInput(genId('name'), isolate(input[[genId('name')]]), label=T("Abundance Index identifier")),
        hr()))

    output$data_tables <- renderUI(do.call(tagList, lapply(grep('^fleet_survey_\\d+_name$', names(input), value = TRUE), function (nf) {
        base_name <- gsub('_name$', '', nf)
        tagList(
            h3(sprintf(T("Quota: %s"), input[[nf]])),
            hodfr::hodfr(paste0('dt_', base_name, '_quota'),
                fields = list(
                    list(name = "year", title = T("Year"), content = "numeric"),
                    list(name = "step", title = T("Step"), content = "numeric"),
                    list(name = "area", title = T("Area")),
                    list(name = "number", title = T("Landings (tonnes)"), content = "numeric")),
                values = list(type = "bins"),
                value = data.frame(year=seq(input$time_year_min, input$time_year_max), step= 1, area="a", number = 4),
                params = list(rowHeaderWidth = 170),
                orientation = 'horizontal'),
            h3(sprintf(T("Landings: %s"), input[[nf]])),
            hodfr::hodfr(paste0('dt_', base_name, '_landings'),
                fields = list(
                    list(name = "number", title = T("Landings"))),
                values = list(type = 'timeseries', min = input$time_start_year, max = input$time_end_year),  #  js_debug = TRUE,
                params = list(rowHeaderWidth = 170),
                orientation = 'horizontal'),
            hr())
    })))
}
