# expand.grid but with groupings right-to-left
rev.expand.grid <- function (...) {
    out <- do.call(expand.grid, rev(list(...)))
    out <- out[,rev(seq_len(ncol(out)))]
    return(out)
}
merge_notnull <- function (x, y, ...) {
    if (is.null(y) || !is.data.frame(y)) x else merge(x, y, ...)
}

# As list(), but with names and values swapped around
list.swapnames <- function (...) {
    inp <- list(...)
    structure(as.list(names(inp)), names = inp)
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
        selectInput(genId('quota'), T("Quota in"), structure(
            c('weight', 'number'),
            names = c(T('Tonnes'), T('Number of individuals'))), selected = isolate(input[[genId('quota')]])),
        div(class="row",
            div(class="col-md-3", selectInput(genId('dist'), T("Landings observations"), list.swapnames(
                none = T('No data'),
                weight = T('Tonnes'),
                number =  T('Number of individuals')), selected = isolate(input[[genId('landings')]]))),
            div(class="col-md-3", selectInput(genId('ldist'), T("Length distribution"), list.swapnames(
                none = T('No data'),
                weight = T('Tonnes'),
                number =  T('Number of individuals')), selected = isolate(input[[genId('ldist')]]))),
            div(class="col-md-3", selectInput(genId('aldist'), T("Age-Length distribution"), list.swapnames(
                none = T('No data'),
                weight = T('Tonnes'),
                number =  T('Number of individuals')), selected = isolate(input[[genId('aldist')]]))),
            ""),
        hr()))

    output$abund <- reactiveSections(input, 'abund', function (genId) tagList(
        textInput(genId('name'), isolate(input[[genId('name')]]), label=T("Abundance Index identifier")),
        div(class="row",
            div(class="col-md-3", selectInput(genId('dist'), T("Landings observations"), list.swapnames(
                none = T('No data'),
                weight = T('Tonnes'),
                number =  T('Number of individuals')), selected = isolate(input[[genId('landings')]]))),
            div(class="col-md-3", selectInput(genId('ldist'), T("Length distribution"), list.swapnames(
                none = T('No data'),
                weight = T('Tonnes'),
                number =  T('Number of individuals')), selected = isolate(input[[genId('ldist')]]))),
            div(class="col-md-3", selectInput(genId('aldist'), T("Age-Length distribution"), list.swapnames(
                none = T('No data'),
                weight = T('Tonnes'),
                number =  T('Number of individuals')), selected = isolate(input[[genId('aldist')]]))),
            ""),
        hr()))

    output$fleets_data <- renderUI(do.call(tabsetPanel, lapply(grep('^(?:fleet|abund)_\\d+_(?:quota|dist|ldist|aldist)$', names(input), value = TRUE), function (df_inp_name) {
        parts <- strsplit(df_inp_name, "_")[[1]]
        base_name <- paste(parts[[1]], parts[[2]], sep = "_")
        df_type <- parts[[3]]
        genId <- function (...) paste(c(base_name, ...), collapse = "_")
        # NB: Assume there's only one stock for now
        genStockId <- function (...) paste(c('stock_1', ...), collapse = "_")

        if (input[[df_inp_name]] == 'none') return(NULL)
        df_fields <- list(
            list(name = "year", title = T("Year"), content = "numeric"),
            list(name = "step", title = T("Step"), content = "numeric"),
            list(name = "area", title = T("Area")))
        df_values <- list(
            year = seq(input$time_year_min, input$time_year_max),
            step = if (isTRUE(input[[genId('step')]] > 0)) input[[genId('step')]] else seq_len(input$time_steps),
            area = input$area_name)

        if (df_type == 'ldist' || df_type == 'aldist') {
            df_fields <- c(df_fields, list(
                list(name = "length", title = T("Length"))))
            df_values <- c(df_values, list(
                length = levels(cut(0, c(seq(
                    input[[genStockId('lg_min')]],
                    input[[genStockId('lg_max')]],
                    input[[genStockId('lg_size')]]), Inf), right = FALSE))))
        }

        if (df_type == 'adist' || df_type == 'aldist') {
            df_fields <- c(df_fields, list(
                list(name = "age", title = T("Age"))))
            df_values <- c(df_values, list(
                age = seq(
                    input[[genStockId('age_min')]],
                    input[[genStockId('age_max')]])))
        }

        if (identical(input[[df_inp_name]], 'weight')) {
            df_fields <- c(df_fields, list(
                list(name = "weight", title = T("Landings (tonnes)"), content = "numeric")))
        } else {
            df_fields <- c(df_fields, list(
                list(name = "number", title = T("Landings (count)"), content = "numeric")))
        }

        tabPanel(
            sprintf("%s: %s", input[[genId('name')]], T(df_type)),
            hodfr::hodfr(
                genId(df_type, 'df'),
                fields = df_fields,
                values = list(type = "bins"),
                value = merge_notnull(
                    do.call(rev.expand.grid, df_values),
                    isolate(input[[genId(df_type, 'df')]]), all.x = TRUE),
                orientation = 'horizontal'))
    })))
}
