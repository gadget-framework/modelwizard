# expand.grid but with groupings right-to-left
rev.expand.grid <- function (...) {
    out <- do.call(expand.grid, rev(list(...)))
    out <- out[,rev(seq_len(ncol(out)))]
    return(out)
}
merge_notnull <- function (x, y, ...) {
    if (is.null(y) || !is.data.frame(y)) x else merge(x, y, ...)
}


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

    output$fleets <- reactiveSections(input, 'fleet', function (genId) tagList(
        textInput(genId('name'), isolate(input[[genId('name')]]), label=T("Fleet identifier")),
        selectInput(genId('step'), T("Active at step"), timestepChoices(), selected = isolate(input[[genId('step')]])),
        selectInput(genId('catchability'), T("Quota in"), structure(
            c('weight', 'number'),
            names = c(T('Tonnes'), T('Number of individuals'))), selected = isolate(input[[genId('catchability')]])),
        selectInput(genId('landings'), T("Landings observations in"), structure(
            c('weight', 'number'),
            names = c(T('Tonnes'), T('Number of individuals'))), selected = isolate(input[[genId('landings')]])),
        hr()))

    output$fleets_data <- renderUI(do.call(tagList, lapply(grep('^fleet_\\d+_name$', names(input), value = TRUE), function (nf) {
        base_name <- gsub('_name$', '', nf)
        genId <- function (x) paste0(base_name, '_', x)
        genDfId <- function (x) paste0('dt_', base_name, '_', x)
        tagList(
            h3(sprintf(T("Quota: %s"), input[[nf]])),
            hodfr::hodfr(
                genDfId('quota'),
                fields = list(
                    list(name = "year", title = T("Year"), content = "numeric"),
                    list(name = "step", title = T("Step"), content = "numeric"),
                    list(name = "area", title = T("Area")),
                    if (input[[genId('catchability')]] == 'weight')
                        list(name = "weight", title = T("Landings (tonnes)"), content = "numeric")
                    else
                        list(name = "number", title = T("Landings (count)"), content = "numeric")),
                values = list(type = "bins"),
                value = merge_notnull(rev.expand.grid(
                    year = seq(input$time_year_min, input$time_year_max),
                    step = if (input[[genId('step')]] > 0) input[[genId('step')]] else seq_len(input$time_steps),
                    area = input$area_name,
                    stringsAsFactors = TRUE), isolate(input[[genDfId('quota')]]), all.x = TRUE),
                orientation = 'horizontal'),
            h3(sprintf(T("Landings: %s"), input[[nf]])),
            hodfr::hodfr(paste0('dt_', base_name, '_landings'),
                fields = list(
                    list(name = "number", title = T("Landings"))),
                values = list(type = "bins"),
                orientation = 'horizontal'),
            hr())
    })))

    output$abundance <- reactiveSections(input, 'abund_idx', function (genId) tagList(
        textInput(genId('name'), isolate(input[[genId('name')]]), label=T("Abundance Index identifier")),
        hr()))
}
