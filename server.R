source('./mw_gadget3.R')
source('./mw_ss.R')

# expand.grid but with groupings right-to-left
rev.expand.grid <- function (...) {
    out <- do.call(expand.grid, rev(list(...)))
    out <- out[,rev(seq_len(ncol(out)))]
    return(out)
}

# As list(), but with names and values swapped around
list.swapnames <- function (...) {
    inp <- list(...)
    structure(as.list(names(inp)), names = inp)
}

# Naive coalesce implementation
coalesce <- function (...) {
    if (!is.null(..1)) return(..1)
    if (!is.null(..2)) return(..2)
    if (!is.null(..3)) return(..3)
    if (!is.null(..4)) return(..4)
    if (!is.null(..5)) return(..5)
    stop("coalesce doesn't support more than 5 arguments")
}

# Wait for renderUI blocks to do their thing, then carry on
# https://github.com/rstudio/shiny/issues/3348#issuecomment-810727477
executeAtNextInput <- function(session = getDefaultReactiveDomain(), values = reactiveValuesToList(session$input), expr) {
  observeEvent(once = TRUE, values, { force(expr) }, ignoreInit = TRUE)
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

data_init_cols <- function (input, df_type, df_unit, base_name) {
    genId <- function (...) paste(c(base_name, ...), collapse = "_")
    # NB: Assume there's only one stock for now
    genStockId <- function (...) paste(c('stock_1', ...), collapse = "_")

    df_names <- c("year", "step", "area")
    if (df_unit == 'none') return(NULL)

    if (df_type == 'adist' || df_type == 'aldist') {
        if (any(!is.finite(c(
            input[[genStockId('age_min')]],
            input[[genStockId('age_max')]])))) return(NULL)
        df_names <- c(df_names, "age")
    }

    if (df_type == 'ldist' || df_type == 'aldist') {
        if (any(!is.finite(c(
            input[[genStockId('lg_min')]],
            input[[genStockId('lg_max')]],
            input[[genStockId('lg_size')]])))) return(NULL)
        df_names <- c(df_names, "length")
    }

    if (identical(df_unit, 'weight')) {
        df_names <- c(df_names, "weight")
    } else {
        df_names <- c(df_names, "number")
    }
    return(df_names)
}
data_cols_to_fields <- function (df_names) {
    structure(list(
        year = list(name = "year", title = T("Year"), content = "numeric"),
        step = list(name = "step", title = T("Step"), content = "numeric"),
        area = list(name = "area", title = T("Area")),
        age = list(name = "age", title = T("Age")),
        length = list(name = "length", title = T("Length")),
        weight = list(name = "weight", title = T("Landings (tonnes)"), content = "numeric"),
        number = list(name = "number", title = T("Landings (count)"), content = "numeric"),
        end = NULL)[unlist(df_names)], names = df_names)
    }
data_init_value <- function (input, df_type, df_unit, base_name) {
    genId <- function (...) paste(c(base_name, ...), collapse = "_")
    # NB: Assume there's only one stock for now
    genStockId <- function (...) paste(c('stock_1', ...), collapse = "_")

    df_values <- list(
        year = seq(input[[genId('year_min')]], input[[genId('year_max')]]),
        step = if (isTRUE(input[[genId('step_active')]] > 0)) input[[genId('step_active')]] else seq_len(input$time_1_steps),
        area = input$area_1_name)

    if (df_type == 'adist' || df_type == 'aldist') {
        if (any(!is.finite(c(
            input[[genStockId('age_min')]],
            input[[genStockId('age_max')]])))) return(NULL)
        df_values <- c(df_values, list(
            age = seq(
                input[[genStockId('age_min')]],
                input[[genStockId('age_max')]])))
    }

    if (df_type == 'ldist' || df_type == 'aldist') {
        if (any(!is.finite(c(
            input[[genStockId('lg_min')]],
            input[[genStockId('lg_max')]],
            input[[genStockId('lg_size')]])))) return(NULL)
        df_values <- c(df_values, list(
            length = levels(cut(0, c(seq(
                input[[genStockId('lg_min')]],
                input[[genStockId('lg_max')]],
                input[[genStockId('lg_size')]]), Inf), right = FALSE))))
    }
    return(do.call(rev.expand.grid, df_values))
}

extractDataFrames <- function (input, spec = TRUE, data = FALSE) {
    extractSingleDataFrame <- function (input, base_name) {
        out <- list()
        for (n in names(input)) {
            if (endsWith(n, "_df")) next
            m <- regmatches(n, regexec(paste0('^', base_name, '_(\\d+)_(.+)'), n))[[1]]
            if (length(m) != 3) next
            i <- as.integer(m[[2]]) ; key <- m[[3]]
            if (endsWith(key, '_prepopulate')) next

            # Place value in one of the list-of-lists tables
            # NB: data.frames don't like gaps, otherwise would use one directly
            if (!(key %in% names(out))) out[[key]] <- list()
            out[[key]][[i]] <- input[[n]]
        }
        if (length(out) == 0) return(data.frame(name = c()))
        return(as.data.frame(lapply(out, unlist)))
    }

    if (spec) {
        out <- c('time', 'area', 'stock', 'fleet', 'abund')
        names(out) <- out
        out <- lapply(out, function (n) extractSingleDataFrame(input, n))
    } else {
        out <- list()
    }

    if (data) {
        # Extract extra data
        for (df_name in names(input)) {
            m <- regmatches(df_name, regexec('^([a-z]+_\\d+)_(.+)_df$', df_name))[[1]]
            if (length(m) != 3) next
            df <- input[[df_name]]

            if (nrow(df) == 1 && all(is.na(df))) {
                # Still has no data, populate it now
                df_inp_name <- gsub('_df$', '', df_name)
                df_type <- m[[3]]
                df_unit <- isolate(input[[df_inp_name]])
                df <- data_init_value(input, df_type, df_unit, m[[2]])
            }

            ws_name <- paste(
                m[[3]],  # Table type
                input[[paste(m[[2]], 'name', sep = "_")]],  # Corresponding fleet_x_name input
                sep = "_")
            out[[ws_name]] <- df
        }
    }

    return(out)
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

    do_file_load <- function (file_path, file_name = basename(file_path)) {
        updateTextInput(session, "file_name", value = gsub('.\\w+$', '', file_name))
        sheet_names <- readxl::excel_sheets(file_path)
        name_mapping <- list()

        # Pass 1: Set counts for sects
        # NB: We don't just switch to be nice, we switch so we can render the UI for pass 2
        shiny::updateTabsetPanel('nav_tabs', 'Specification', session = session)
        session$sendCustomMessage("selectTab", "second_tab")
        for (n in names(sect)) {
            df <- as.data.frame(readxl::read_excel(file_path, n, na = c("", "NA")))
            sect[[n]]$count(0)
            sect[[n]]$count(nrow(df))
            # Temporarily increase step count to maximum,
            # updating available step_active choices now so we don't choose an invalid value
            updateSelectInput(session, 'time_1_steps', selected = 12)
        }

        # Pass 2 (after UI recalculated): Set sect values
        executeAtNextInput(session, expr = {
            for (n in c('time', 'area', 'stock', 'fleet', 'abund')) {
                df <- as.data.frame(readxl::read_excel(file_path, n))
                if ('name' %in% names(df) && length(df$name) > 0) {
                    # Add table's names to name mapping
                    name_mapping <- c(name_mapping, structure(
                        paste(n, seq_len(nrow(df)), sep = "_"),
                        names = df$name))
                }

                for (row_n in seq_len(nrow(df))) {
                    for (col_n in names(df)) {
                        inp_name <- paste(c(n, row_n, col_n), collapse = "_")
                        inp_value <- df[as.integer(row_n), col_n]
                        updateTextInput(session, inp_name, value = inp_value)
                    }
                }
            }

            # Pass 3: Set data.frame values
            executeAtNextInput(session, expr = {
                for (n in sheet_names) {
                    m <- regmatches(n, regexec('^([a-z]+)_(.+)', n))[[1]]
                    if (length(m) != 3) next

                    df <- as.data.frame(readxl::read_excel(file_path, n), stringsAsFactors = TRUE)
                    df_fields <- data_cols_to_fields(names(df))
                    unknown_fields <- Filter(is.null, df_fields)
                    if (length(unknown_fields) > 0) {
                        stop("Unknown fields in data ", n, ": ", paste(names(unknown_fields), collapse = ", "))
                    }
                    # TODO: pass in fields?

                    df_name <- paste(name_mapping[[m[[3]]]], m[[2]], 'df', sep = "_")
                    hodfr::updateHodfrInput(session, df_name, value = df)
                }
            })
        })
    }
    observeEvent(input$file_load_demo_act, {
         do_file_load('./anch.xlsx')
    })
    observeEvent(input$file_load, {
        do_file_load(input$file_load$datapath, input$file_load$name)
    })

    output$file_save_act <- downloadHandler(filename = function() paste0(input$file_name, ".xlsx"), content = function(file) {
        writexl::write_xlsx(extractDataFrames(input,
            spec = TRUE,
            data = TRUE), path = file)
    })
    output$file_save_g3_act <- downloadHandler(filename = function() paste0(input$file_name, ".xlsx"), content = function(file) {
        writexl::write_xlsx(extractDataFrames(input,
            spec = TRUE,
            data = TRUE), path = file)
    })
    output$file_save_ss_act <- downloadHandler(filename = function() paste0(input$file_name, ".xlsx"), content = function(file) {
        writexl::write_xlsx(extractDataFrames(input,
            spec = TRUE,
            data = TRUE), path = file)
    })

    # Stocks ##################################################################

    sect$stock <- reactiveSections(input, 'stock', function (genId) tagList(
        textInput(genId('name'), isolate(input[[genId('name')]]), label=T("Identifier")),
        p(class="help-block", T("An identifier to name the species within the model. Letters, numbers and underscore are allowed.")),
        div(class="row",
            div(class="col-md-3", numericInput(genId('lg_min'), T("Minimum length group"), isolate(input[[genId('lg_min')]]))),
            div(class="col-md-3", numericInput(genId('lg_max'), T("Maximum length group"), isolate(input[[genId('lg_max')]]))),
            div(class="col-md-3", numericInput(genId('lg_size'), T("Length group size"), isolate(input[[genId('lg_size')]]))),
            ""),
        p(class="help-block", T("Length bins for your stock. The final group in the model will be maximum..Inf.")),
        div(class="row",
            div(class="col-md-3", numericInput(genId('age_min'), T("Minimum age"), isolate(input[[genId('age_min')]]))),
            div(class="col-md-3", numericInput(genId('age_max'), T("Maximum age"), isolate(input[[genId('age_max')]]))),
            ""),
        p(class="help-block", T("Age bins for your stock.")),
        hideIfOneTimestep(
            selectInput(genId('renewal_step'), T("Renewal at step"), timestepChoices(), selected = isolate(input[[genId('renewal_step')]]))),
        hr()), default_count = 1, button_add = FALSE, button_remove = FALSE)
    output$stocks <- sect$stock$ui

    # Fleet / abundance indices ###############################################

    sect$fleet <- reactiveSections(input, 'fleet', function (genId) tagList(
        textInput(genId('name'), isolate(input[[genId('name')]]), label=T("identifier")),
        p(class="help-block", T("An identifier to name the fleet/survey within the model. Letters, numbers and underscore are allowed.")),
        div(class="row",
            div(class="col-md-3", numericInput(genId('year_min'), isolate(coalesce(
                input[[genId('year_min')]],
                input[['time_1_year_min']],
                1990)), label=T("Start year for fleet"))),
            div(class="col-md-3", numericInput(genId('year_max'), isolate(coalesce(
                input[[genId('year_max')]],
                input[['time_1_year_max']],
                1999)), label=T("End year for fleet"))),
            ""),
        p(class="help-block", T("Years that this fleet will be active / data is available for. Should be within overall model years above.")),
        hideIfOneTimestep(tagList(
            selectInput(genId('step_active'), T("Active at step"), timestepChoices(), selected = isolate(input[[genId('step_active')]])),
            p(class="help-block", T("If the fleet is only active in one step/season in the year, choose it here.")),
            span())),
        selectInput(genId('landings'), T("Landings in"), structure(
            c('weight', 'number'),
            names = c(T('Tonnes'), T('Number of individuals'))), selected = isolate(input[[genId('landings')]])),
        p(class="help-block", T("The unit the landings data for this fleet will be provided in on the next tab.")),
        div(class="row",
            div(class="col-md-3", selectInput(genId('ldist'), T("Length distribution"), list.swapnames(
                none = T('No data'),
                weight = T('Tonnes'),
                number =  T('Number of individuals')), selected = isolate(input[[genId('ldist')]]))),
            div(class="col-md-3", selectInput(genId('aldist'), T("Age-Length distribution"), list.swapnames(
                none = T('No data'),
                weight = T('Tonnes'),
                number =  T('Number of individuals')), selected = isolate(input[[genId('aldist')]]))),
            ""),
        p(class="help-block", T("If age or age-length distribution data is available, select the relevant option and fill in the data in the next tab.")),
        hr()))
    output$fleets <- sect$fleet$ui

    sect$abund <- reactiveSections(input, 'abund', function (genId) tagList(
        textInput(genId('name'), isolate(input[[genId('name')]]), label=T("Abundance Index identifier")),
        p(class="help-block", T("An identifier to name the abundance index within the model. Letters, numbers and underscore are allowed.")),
        div(class="row",
            div(class="col-md-3", numericInput(genId('year_min'), isolate(coalesce(
                input[[genId('year_min')]],
                input[['time_1_year_min']],
                1990)), label=T("Start year for survey"))),
            div(class="col-md-3", numericInput(genId('year_max'), isolate(coalesce(
                input[[genId('year_max')]],
                input[['time_1_year_max']],
                1999)), label=T("End year for survey"))),
            ""),
        p(class="help-block", T("Years that this survey will be active / data is available for. Should be within overall model years above.")),
        hideIfOneTimestep(tagList(
            selectInput(genId('step_active'), T("Active at step"), timestepChoices(), selected = isolate(input[[genId('step_active')]])),
            p(class="help-block", T("If the survey is only performed in one step/season in the year, choose it here.")),
            span())),
        div(class="row",
            div(class="col-md-3", selectInput(genId('dist'), T("Aggregated observations"), list.swapnames(
                none = T('No data'),
                weight = T('Tonnes'),
                number =  T('Number of individuals')), selected = isolate(input[[genId('dist')]]))),
            div(class="col-md-3", selectInput(genId('ldist'), T("Length distribution"), list.swapnames(
                none = T('No data'),
                weight = T('Tonnes'),
                number =  T('Number of individuals')), selected = isolate(input[[genId('ldist')]]))),
            div(class="col-md-3", selectInput(genId('aldist'), T("Age-Length distribution"), list.swapnames(
                none = T('No data'),
                weight = T('Tonnes'),
                number =  T('Number of individuals')), selected = isolate(input[[genId('aldist')]]))),
            ""),
        p(class="help-block", T("Select which kinds of abudance distribution data are available and fill in the data in the next tab.")),
        hr()))
    output$abund <- sect$abund$ui

    # Fleet data ##############################################################

    output$all_data <- renderUI(do.call(tabsetPanel, c(list(id = "all_data_tabs"), lapply(grep('^(?:fleet|abund)_\\d+_(?:landings|dist|ldist|aldist)$', names(input), value = TRUE), function (df_inp_name) {
        parts <- strsplit(df_inp_name, "_")[[1]]
        base_name <- paste(parts[[1]], parts[[2]], sep = "_")
        df_type <- parts[[3]]
        df_unit <- input[[df_inp_name]]
        df_name <- paste0(df_inp_name, '_df')
        genId <- function (...) paste(c(base_name, ...), collapse = "_")

        init_cols <- data_init_cols(input, df_type, df_unit, base_name)
        if (is.null(init_cols)) return(NULL)

        df <- isolate(input[[df_name]])
        if (is.null(df)) df <- data.frame(x = NA)
        if (!identical(init_cols, names(df))) {
            for (extra in setdiff(init_cols, names(df))) {
                # Fill in missing columns with NA
                df[[extra]] <- NA
            }
            # Select only the columns we're now interested in
            df <- df[, init_cols, drop = FALSE]
        }
        observeEvent(input[[genId(df_type, 'prepopulate')]], {
            df <- data_init_value(input, df_type, df_unit, base_name)
            hodfr::updateHodfrInput(session, df_name, value = df)
        })
        # Set initial data
        hodfr::updateHodfrInput(session, df_name, value = df)
        tabPanel(
            sprintf("%s: %s", input[[genId('name')]], T(df_type)),
            value = genId(df_type, 'tab'),
            actionButton(genId(df_type, 'prepopulate'), T("Clear & prepopulate values")),
            hodfr::hodfr(
                df_name,
                fields = unname(data_cols_to_fields(names(df))),
                values = list(type = "bins"),
                orientation = 'horizontal'))
    }))))
    observeEvent(c(input$nav_tabs, input$all_data_tabs), if (isolate(input$nav_tabs) == "data") {
        df_inp_name <- gsub('_tab$', '', isolate(input$all_data_tabs))
        df_name <- paste0(df_inp_name, '_df')

        df <- isolate(input[[df_name]])
        if (nrow(df) == 1 && all(is.na(df))) {
            parts <- strsplit(df_inp_name, "_")[[1]]
            base_name <- paste(parts[[1]], parts[[2]], sep = "_")
            df_type <- parts[[3]]
            df_unit <- isolate(input[[df_inp_name]])

            # Still has no data, populate it now
            df <- data_init_value(input, df_type, df_unit, base_name)
            hodfr::updateHodfrInput(session, df_name, value = df)
        } else {
            # NB: handsondataframe won't render properly if table isn't visible,
            # so explicitly tell the targeted tab to re-render when we switch
            hodfr::renderHodfrInput(session, df_name)
        }
    })
    # Always render data, so if we hit save without visiting the tab it's been computed
    outputOptions(output, "all_data", suspendWhenHidden = FALSE)

    # Gadget3 script tab ######################################################
    observeEvent(input$nav_tabs, if (input$nav_tabs == 'script_g3') {
        tryCatch({
            model_env <- list2env(extractDataFrames(input, data = TRUE), parent = asNamespace("gadget3"))
            model_env$script <- mw_g3_script(
                spec = model_env,
                compile = TRUE,
                run = FALSE)
            saveRDS(model_env, file = '/tmp/model_env.rds')
            eval(parse(text = model_env$script), envir = model_env)

            # Now we know the model is sane enough, display it
            output$script_g3_text <- renderText(mw_g3_script(
                spec = extractDataFrames(input, data = FALSE),
                xlsx = paste0(input$file_name, ".xlsx"),
                compile = TRUE,
                run = TRUE))
        }, error = function(e) {
            output$script_g3_text <- renderText(paste(
                "** Cannot create model **", "",
                e$message,
                deparse1(e$call),
                sep = "\n"))
        })
    })

    # SS3 script tab ######################################################
    observeEvent(input$nav_tabs, if (input$nav_tabs == 'script_ss') {
        output$script_ss_text <- renderText(mw_ss_script(
            spec = extractDataFrames(input, data = FALSE),
            xlsx = paste0(input$file_name, ".xlsx")))
    })
}
