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
    observeEvent(input[[paste0(val_name, '_add_act')]], { rv(rv() + 1) })
    observeEvent(input[[paste0(val_name, '_remove_act')]], { rv(max(0, rv() - 1)) })
    return(list(count = rv, ui = renderUI(do.call(tagList, c(
        lapply(seq_len(rv()), function (i) {
            genId <- function (s) sprintf('%s_%d_%s', val_name, i, s)
            return(ui_func(genId))
        }),
        list(
            if (button_add) actionButton(paste0(val_name, '_add_act'), T("Add new")) else "",
            if (button_remove) actionButton(paste0(val_name, '_remove_act'), T("Remove")) else "",
            ""))))))
}

server <- function(input, output, session) {
    timestepChoices <- reactive(structure(
        as.list(seq(0, input$time_1_steps)),
        names = c(T("Every timestep"), seq_len(input$time_1_steps))))
    hideIfOneTimestep <- function (...) {
        div(..., style=if (input$time_1_steps == 1) 'display: none' else '')
    }
    sect <- list()

    # File I/O ################################################################

    observeEvent(input$file_load_act, {
        updateTextInput(session, "filename", value = gsub('.\\w+$', '', input$loadFile$name))
    })
    output$file_save_act <- downloadHandler(filename = function() paste0(input$file_name, ".xlsx"), content = function(file) {
        wb <- openxlsx::createWorkbook()

        # Create initial sheets matching specification tables
        specs <- list(time = list(), area = list(), stock = list(), fleet = list(), abund = list())
        for (n in names(specs)) openxlsx::addWorksheet(wb, sheetName = n)

        for (n in names(input)) {
            if (startsWith(n, "file_") || endsWith(n, "_act") || endsWith(n, "_tabs")) {
                # Ignore navigation / tab controls
            } else if (identical(n, "max")) {
                # Ignore HODFR dimension inputs
            } else if (endsWith(n, "_df")) {
                m <- regmatches(n, regexec('^([a-z]+_\\d+)_(.+)_df$', n))[[1]]
                if (length(m) != 3) stop("Unparsable input name: ", n)
                ws_name <- paste(
                    m[[3]],  # Table type
                    input[[paste(m[[2]], 'name', sep = "_")]],  # Corresponding fleet_x_name input
                    sep = "_")

                openxlsx::addWorksheet(wb, sheetName = ws_name)
                openxlsx::writeDataTable(wb, sheet = ws_name, input[[n]])
            } else {
                m <- regmatches(n, regexec('^([a-z]+)_(\\d+)_(.+)', n))[[1]]
                if (length(m) != 4) stop("Unparsable input name: ", n)
                sect <- m[[2]] ; i <- m[[3]] ; key <- m[[4]]

                # Place value in one of the list-of-lists tables
                # NB: data.frames don't like gaps, otherwise would use one directly
                if (!(key %in% names(specs[[sect]]))) specs[[sect]][[key]] <- list()
                specs[[sect]][[key]][[i]] <- input[[n]]
            }
        }
        for (n in names(specs)) {
            if (length(specs[[n]]) == 0) {
                openxlsx::writeData(wb, sheet = n, "name")
            } else {
                df <- as.data.frame(lapply(specs[[n]], unlist))
                openxlsx::writeDataTable(wb, sheet = n, df)
            }
        }
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    })

    # Stocks ##################################################################

    sect$stock <- reactiveSections(input, 'stock', function (genId) tagList(
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
        hideIfOneTimestep(
            selectInput(genId('renewal_step'), T("Renewal at step"), timestepChoices(), selected = isolate(input[[genId('step')]]))),
        hr()), default_count = 1, button_add = FALSE, button_remove = FALSE)
    output$stocks <- sect$stock$ui

    # Fleet / abundance indices ###############################################

    sect$fleet <- reactiveSections(input, 'fleet', function (genId) tagList(
        textInput(genId('name'), isolate(input[[genId('name')]]), label=T("Fleet identifier")),
        hideIfOneTimestep(
            selectInput(genId('step'), T("Active at step"), timestepChoices(), selected = isolate(input[[genId('step')]]))),
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
    output$fleets <- sect$fleet$ui

    sect$abund <- reactiveSections(input, 'abund', function (genId) tagList(
        textInput(genId('name'), isolate(input[[genId('name')]]), label=T("Abundance Index identifier")),
        hideIfOneTimestep(
            selectInput(genId('step'), T("Active at step"), timestepChoices(), selected = isolate(input[[genId('step')]]))),
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
    output$abund <- sect$abund$ui

    # Fleet data ##############################################################

    output$fleets_data <- renderUI(do.call(tabsetPanel, c(list(id = "fleets_data_tabs"), lapply(grep('^(?:fleet|abund)_\\d+_(?:quota|dist|ldist|aldist)$', names(input), value = TRUE), function (df_inp_name) {
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
            year = seq(input$time_1_year_min, input$time_1_year_max),
            step = if (isTRUE(input[[genId('step')]] > 0)) input[[genId('step')]] else seq_len(input$time_1_steps),
            area = input$area_1_name)

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
            value = genId(df_type, 'tab'),
            hodfr::hodfr(
                genId(df_type, 'df'),
                fields = df_fields,
                values = list(type = "bins"),
                value = merge_notnull(
                    do.call(rev.expand.grid, df_values),
                    isolate(input[[genId(df_type, 'df')]]), all.x = TRUE),
                orientation = 'horizontal'))
    }))))
    observeEvent(input$fleets_data_tabs, {
        # NB: handsondataframe won't render properly if table isn't visible,
        # so explicitly tell the targeted tab to re-render when we switch
        hodfr::renderHodfrInput(session, gsub('_tab$', '_df', input$fleets_data_tabs))
    })
    # Always render data, so if we hit save without visiting the tab it's been computed
    outputOptions(output, "fleets_data", suspendWhenHidden = FALSE)
}
