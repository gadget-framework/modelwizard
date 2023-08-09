source('./mw_gadget3.R')

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

extractDataFrames <- function (input, spec = TRUE, data = FALSE, params = FALSE) {
    extractSingleDataFrame <- function (input, base_name) {
        out <- list()
        for (n in names(input)) {
            if (endsWith(n, "_df")) next
            m <- regmatches(n, regexec(paste0('^', base_name, '_(\\d+)_(.+)'), n))[[1]]
            if (length(m) != 3) next
            i <- as.integer(m[[2]]) ; key <- m[[3]]

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
        for (n in names(input)) {
            m <- regmatches(n, regexec('^([a-z]+_\\d+)_(.+)_df$', n))[[1]]
            if (length(m) != 3) next
            ws_name <- paste(
                m[[3]],  # Table type
                input[[paste(m[[2]], 'name', sep = "_")]],  # Corresponding fleet_x_name input
                sep = "_")
            out[[ws_name]] <- input[[n]]
        }
    }

    if (params) {
        out[['params']] <- input[['params']]
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

    observeEvent(input$file_load, {
        updateTextInput(session, "file_name", value = gsub('.\\w+$', '', input$file_load$name))
        sheet_names <- readxl::excel_sheets(input$file_load$datapath)
        name_mapping <- list()

        # Pass 1: Set counts for sects
        for (n in names(sect)) {
            df <- as.data.frame(readxl::read_excel(input$file_load$datapath, n))
            sect[[n]]$count(0)
            sect[[n]]$count(nrow(df))
        }

        # Pass 2 (after UI recalculated): Set sect values
        executeAtNextInput(session, expr = {
            for (n in c('time', 'area', 'stock', 'fleet', 'abund')) {
                df <- as.data.frame(readxl::read_excel(input$file_load$datapath, n))
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

                    df <- as.data.frame(readxl::read_excel(input$file_load$datapath, n), stringsAsFactors = TRUE)
                    df_name <- paste(name_mapping[[m[[3]]]], m[[2]], 'df', sep = "_")
                    hodfr::updateHodfrInput(session, df_name, value = df)
                }
                if ('params' %in% sheet_names) {
                    df <- as.data.frame(readxl::read_excel(input$file_load$datapath, 'params'), stringsAsFactors = FALSE)
                    df$optimise <- df$optimise != 0
                    hodfr::updateHodfrInput(session, 'params', value = df)
                }
            })
        })
    })

    output$file_save_act <- downloadHandler(filename = function() paste0(input$file_name, ".xlsx"), content = function(file) {
        writexl::write_xlsx(extractDataFrames(input,
            spec = TRUE,
            params = TRUE,
            data = TRUE), path = file)
    })

    # Stocks ##################################################################

    sect$stock <- reactiveSections(input, 'stock', function (genId) tagList(
        textInput(genId('name'), isolate(input[[genId('name')]]), label=T("Identifier")),
        div(class="row",
            div(class="col-md-3", numericInput(genId('lg_min'), T("Minimum length group"), isolate(input[[genId('lg_min')]]))),
            div(class="col-md-3", numericInput(genId('lg_max'), T("Maximum length group"), isolate(input[[genId('lg_max')]]))),
            div(class="col-md-3", numericInput(genId('lg_size'), T("Length group size"), isolate(input[[genId('lg_size')]]))),
            ""),
        div(class="row",
            div(class="col-md-3", numericInput(genId('age_min'), T("Minimum age"), isolate(input[[genId('age_min')]]))),
            div(class="col-md-3", numericInput(genId('age_max'), T("Maximum age"), isolate(input[[genId('age_max')]]))),
            ""),
        hideIfOneTimestep(
            selectInput(genId('renewal_step'), T("Renewal at step"), timestepChoices(), selected = isolate(input[[genId('step')]]))),
        hr()), default_count = 1, button_add = FALSE, button_remove = FALSE)
    output$stocks <- sect$stock$ui

    # Fleet / abundance indices ###############################################

    sect$fleet <- reactiveSections(input, 'fleet', function (genId) tagList(
        textInput(genId('name'), isolate(input[[genId('name')]]), label=T("Fleet identifier")),
        hideIfOneTimestep(
            selectInput(genId('step_active'), T("Active at step"), timestepChoices(), selected = isolate(input[[genId('step_active')]]))),
        selectInput(genId('landings'), T("Landings in"), structure(
            c('weight', 'number'),
            names = c(T('Tonnes'), T('Number of individuals'))), selected = isolate(input[[genId('landings')]])),
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
        hr()))
    output$fleets <- sect$fleet$ui

    sect$abund <- reactiveSections(input, 'abund', function (genId) tagList(
        textInput(genId('name'), isolate(input[[genId('name')]]), label=T("Abundance Index identifier")),
        hideIfOneTimestep(
            selectInput(genId('step_active'), T("Active at step"), timestepChoices(), selected = isolate(input[[genId('step_active')]]))),
        div(class="row",
            div(class="col-md-3", selectInput(genId('dist'), T("Aggregated observations"), list.swapnames(
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

    output$fleets_data <- renderUI(do.call(tabsetPanel, c(list(id = "fleets_data_tabs"), lapply(grep('^(?:fleet|abund)_\\d+_(?:landings|dist|ldist|aldist)$', names(input), value = TRUE), function (df_inp_name) {
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

        if (df_type == 'adist' || df_type == 'aldist') {
            if (any(!is.finite(c(
                input[[genStockId('age_min')]],
                input[[genStockId('age_max')]])))) return(NULL)
            df_fields <- c(df_fields, list(
                list(name = "age", title = T("Age"))))
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
            df_fields <- c(df_fields, list(
                list(name = "length", title = T("Length"))))
            df_values <- c(df_values, list(
                length = levels(cut(0, c(seq(
                    input[[genStockId('lg_min')]],
                    input[[genStockId('lg_max')]],
                    input[[genStockId('lg_size')]]), Inf), right = FALSE))))
        }

        if (identical(input[[df_inp_name]], 'weight')) {
            df_fields <- c(df_fields, list(
                list(name = "weight", title = T("Landings (tonnes)"), content = "numeric")))
        } else {
            df_fields <- c(df_fields, list(
                list(name = "number", title = T("Landings (count)"), content = "numeric")))
        }

        df <- isolate(input[[genId(df_type, 'df')]])
        if (is.null(df)) {
            df <- do.call(rev.expand.grid, df_values)
        } else {
            df <- merge(do.call(rev.expand.grid, df_values), df, all.x = TRUE)
        }
        tabPanel(
            sprintf("%s: %s", input[[genId('name')]], T(df_type)),
            value = genId(df_type, 'tab'),
            hodfr::hodfr(
                genId(df_type, 'df'),
                fields = df_fields,
                values = list(type = "bins"),
                value = df,
                orientation = 'horizontal'))
    }))))
    observeEvent(input$nav_tabs, if (input$nav_tabs == "data") {
        # NB: handsondataframe won't render properly if table isn't visible,
        # so explicitly tell the targeted tab to re-render when we switch
        hodfr::renderHodfrInput(session, gsub('_tab$', '_df', input$fleets_data_tabs))
    })
    observeEvent(input$fleets_data_tabs, {
        # NB: handsondataframe won't render properly if table isn't visible,
        # so explicitly tell the targeted tab to re-render when we switch
        hodfr::renderHodfrInput(session, gsub('_tab$', '_df', input$fleets_data_tabs))
    })
    # Always render data, so if we hit save without visiting the tab it's been computed
    outputOptions(output, "fleets_data", suspendWhenHidden = FALSE)

    # Model parameters ########################################################
    observeEvent(input$nav_tabs, if (input$nav_tabs == 'parameters') {
        tryCatch({
            model_env <- list2env(extractDataFrames(input, params = TRUE, data = TRUE), parent = asNamespace("gadget3"))
            model_env$script <- mw_g3_script(
                spec = model_env,
                compile = TRUE,
                run = FALSE)
            saveRDS(model_env, file = '/tmp/model_env.rds')
            eval(parse(text = model_env$script), envir = model_env)
            df <- model_env$params.in[,c('switch', 'value', 'optimise', 'lower', 'upper')]
            df$value <- unlist(df$value)  # NB: HODF won't display list columns, we shouldn't have any parameter vectors
            rownames(df) <- NULL  # NB: Without, HODF gets confused as to why our rownames don't match

            if (!is.null(input$params)) {
                existing_df <- input$params
                df <- rbind(
                    # Filter out old parameters from existing table
                    existing_df[existing_df$switch %in% df$switch,],
                    # Concatenate any new parameters
                    df[!(df$switch %in% existing_df$switch),],
                    make.row.names = FALSE)
                df$optimise <- df$optimise != 0
            }

            hodfr::updateHodfrInput(session, 'params', value = df)
        }, error = function(e) {
            str(e)
            output$parameters_error <- renderText(paste(e$message, deparse1(e$call), sep = "\n"))
            hodfr::updateHodfrInput(session, 'params', value = data.frame(switch = c(), value = c(),optimise = c(),lower = c(),upper = c()))
        })
    })

    # Gadget3 script tab ######################################################
    observeEvent(input$nav_tabs, if (input$nav_tabs == 'script_g3') {
        output$script_g3_text <- renderText(mw_g3_script(
            spec = extractDataFrames(input, data = FALSE),
            xlsx = paste0(input$file_name, ".xlsx"),
            compile = TRUE,
            run = TRUE))
    })
}
