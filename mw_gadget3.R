library(gadget3)  # NB: This is to force shinyapps to register the dependency
library(gadgetutils)  # NB: This is to force shinyapps to register the dependency

template_str <- function (s) {
    stringr::str_interp(s, parent.frame(1))
}
escape_sym <- Vectorize(function (s) deparse1(as.symbol(s), backtick = TRUE))

mw_g3_code_header <- function (spec, xlsx, compile = FALSE, run = FALSE) {
    libs <- c(
        'gadget3',
        (if (nzchar(xlsx)) 'readxl' else NULL),
        (if (compile) "gadgetutils" else NULL),
        (if (run) "gadgetplots" else NULL),
        NULL)
    template_str(r'(
${paste("library(", libs, ")", sep = "", collapse = "\n")}

actions <- list()
data_path <- ${deparse1(xlsx)}
)')}

mw_g3_code_readxl <- function (sheet_name, xlsx) {
    if (!nzchar(xlsx)) return("")
    template_str('${escape_sym(sheet_name)} <- read_excel(data_path, ${deparse1(sheet_name)}, na = c("", "NA"))\n')
}

mw_g3_code_area <- function (spec) {
    area_names <- seq_along(spec$area$name)
    names(area_names) <- spec$area$name

    template_str(r'(
# Create area definitions ####################
area_names <- ${deparse1(area_names)}
)')}

mw_g3_code_time <- function (r, spec) {
    year_max <- as.integer(r$year_max)
    year_min <- as.integer(r$year_min)
    steps_count <- as.integer(r$steps)
    step_lengths <- rep(as.integer(12 / steps_count), steps_count)

    template_str(r'(
# Create time definitions ####################

actions_time <- list(
  g3a_time(
    ${deparse1(year_min)}, ${deparse1(year_max)},
    step_lengths = ${deparse1(step_lengths)}),
  NULL)

actions <- c(actions, actions_time)
)')}

mw_g3_code_stock <- function (r, spec, xlsx) {
    stock_sym <- escape_sym(r$name)
    actions_sym <- escape_sym(paste("actions", r$name, sep = "_"))
    actions_likelihood_sym <- escape_sym(paste("actions", "likelihood", r$name, sep = "_"))
    area_names <- spec$area$name
    template_str(r'(
# Create stock definition for ${r$name} ####################
${stock_sym} <- g3_stock(${deparse1(r$name)}, seq(${deparse1(r$lg_min)}, ${deparse1(r$lg_max)}, ${deparse(r$lg_size)})) |>
  g3s_livesonareas(area_names[${deparse1(area_names)}]) |>
  g3s_age(${deparse1(as.integer(r$age_min))}, ${deparse1(as.integer(r$age_max))})

${actions_sym} <- list(
  g3a_growmature(${stock_sym}, g3a_grow_impl_bbinom(
    maxlengthgroupgrowth = ${deparse1(as.integer((r$lg_max - r$lg_min) / r$lg_size))})),
  g3a_naturalmortality(${stock_sym}),
  g3a_initialconditions_normalcv(${stock_sym}),
  g3a_renewal_normalparam(${stock_sym},
    run_step = ${deparse1(if (r$renewal_step == 0) NULL else r$renewal_step)}),
  g3a_age(${stock_sym}),
  NULL)

${actions_likelihood_sym} <- list(
  g3l_understocking(list(${stock_sym}), weight = 1e+08, nll_breakdown = TRUE),
  NULL)

actions <- c(actions, ${actions_sym}, ${actions_likelihood_sym})
)')}

mw_g3_code_readxl_dist <- function (dist_type, r, xlsx) {
    if (is.null(r[[dist_type]]) || r[[dist_type]] == "none") return("")
    lc_name <- unname(paste(dist_type, r$name, sep = "_"))
    mw_g3_code_readxl(lc_name, xlsx)
}

mw_g3_code_likelihood_dist <- function (dist_type, r, spec) {
    if (is.null(r[[dist_type]]) || r[[dist_type]] == "none") return("")

    fleet_list <- list(as.symbol(r$name))
    stock_list <- lapply(spec$stock$name, as.symbol)
    lc_name <- unname(paste(dist_type, r$name, sep = "_"))
    data_sym <- escape_sym(lc_name)
    is_abundance <- is.null(r$landings)

    template_str(r'(
  g3l_${if (is_abundance) "abundance" else "catch"}distribution(
    ${deparse1(lc_name)},
    ${data_sym},
    ${if (!is_abundance) paste0("fleets = ", deparse1(fleet_list, backtick = TRUE), ",") else ""}
    stocks = ${deparse1(stock_list, backtick = TRUE)},
    function_f = ${if (is_abundance) "g3l_distribution_surveyindices_log(alpha = NULL, beta = 1)" else "g3l_distribution_sumofsquares()"},
    area_group = area_names,
    report = TRUE,
    nll_breakdown = TRUE),)')}

mw_g3_code_fleet <- function (r, spec, xlsx) {
    fleet_sym <- escape_sym(r$name)
    actions_sym <- escape_sym(paste("actions", r$name, sep = "_"))
    actions_likelihood_sym <- escape_sym(paste("actions", "likelihood", r$name, sep = "_"))
    area_names <- spec$area$name
    stock_list <- lapply(spec$stock$name, as.symbol)
    data_name <- paste("landings", r$name, sep = "_")
    data_sym <- escape_sym(data_name)

    template_str(r'(
# Create fleet definition for ${r$name} ####################
${fleet_sym} <- g3_fleet(${deparse1(r$name)}) |> g3s_livesonareas(area_names[${deparse1(area_names)}])

${mw_g3_code_readxl(data_name, xlsx)}${mw_g3_code_readxl_dist("dist", r, xlsx)}${mw_g3_code_readxl_dist("ldist", r, xlsx)}${mw_g3_code_readxl_dist("aldist", r, xlsx)}
${actions_sym} <- list(
  g3a_predate_fleet(
    ${fleet_sym},
    ${deparse1(stock_list, backtick = TRUE)},
    suitabilities = g3_suitability_exponentiall50(),
    catchability_f = g3a_predate_catchability_numberfleet(
      g3_timeareadata(${deparse1(data_name)}, ${data_sym}, ${deparse1(r$landings)}, areas = area_names))),
  NULL)
${actions_likelihood_sym} <- list(${mw_g3_code_likelihood_dist("dist", r, spec)}${mw_g3_code_likelihood_dist("ldist", r, spec)}${mw_g3_code_likelihood_dist("aldist", r, spec)}
  NULL)

actions <- c(actions, ${actions_sym}, ${actions_likelihood_sym})
)')}

mw_g3_code_abund <- function (r, spec, xlsx) {
    fleet_sym <- escape_sym(r$name)
    actions_sym <- escape_sym(paste("actions", r$name, sep = "_"))
    actions_likelihood_sym <- escape_sym(paste("actions", "likelihood", r$name, sep = "_"))
    stock_list <- lapply(spec$stock$name, as.symbol)

    template_str(r'(
# Create abundance index for ${r$name} ####################
${mw_g3_code_readxl_dist("dist", r, xlsx)}${mw_g3_code_readxl_dist("ldist", r, xlsx)}${mw_g3_code_readxl_dist("aldist", r, xlsx)}
${actions_sym} <- list(
  NULL)
${actions_likelihood_sym} <- list(
${mw_g3_code_likelihood_dist("dist", r, spec)}${mw_g3_code_likelihood_dist("ldist", r, spec)}${mw_g3_code_likelihood_dist("aldist", r, spec)}
  NULL)

actions <- c(actions, ${actions_sym}, ${actions_likelihood_sym})
)')}

mw_g3_code_compile <- function (spec, xlsx) {
    stock_list <- lapply(spec$stock$name, as.symbol)
    template_str(r'(
# Create model objective function ####################

model_code <- g3_to_tmb(actions)

# Guess l50 / linf based on stock sizes
estimate_l50 <- gadget3::g3_stock_def(${deparse1(stock_list[[1]], backtick = TRUE)}, "midlen")[[length(gadget3::g3_stock_def(${deparse1(stock_list[[1]], backtick = TRUE)}, "midlen")) / 2]]
estimate_linf <- max(gadget3::g3_stock_def(${deparse1(stock_list[[1]], backtick = TRUE)}, "midlen"))
estimate_t0 <- gadget3::g3_stock_def(st, "minage") - 0.8

params.in <- attr(model_code, "parameter_template") |>
  gadgetutils::g3_init_guess("\\.(rec|init)\\.scalar$", 10, 0.001, 200, 1) |>
  gadgetutils::g3_init_guess("\\.init\\.[0-9]+$", 10, 0.001, 200, 1) |>
  gadgetutils::g3_init_guess("\\.rec\\.[0-9]+$", 100, 1e-6, 1000, 1) |>
  gadgetutils::g3_init_guess("\\.rec.sd$", 5, 4, 20, 1) |>
  gadgetutils::g3_init_guess("\\.M$", 0.15, 0.001, 1, 1) |>
  gadgetutils::g3_init_guess("^init\\.F$", 0.5, 0.1, 1, 1) |>
  gadgetutils::g3_init_guess("\\.Linf$", estimate_linf, estimate_linf * (1 - 0.2), estimate_linf * (1 + 0.2), 1) |>
  gadgetutils::g3_init_guess("\\.K$", 0.3, 0.04, 1.2, 1) |>
  gadgetutils::g3_init_guess("\\.t0$", estimate_t0, estimate_t0 * (1 - 2), estimate_t0 * (1 + 2), 1) |>
  gadgetutils::g3_init_guess("\\.walpha$", 0.01) |>
  gadgetutils::g3_init_guess("\\.wbeta$", 3) |>
  gadgetutils::g3_init_guess("^\\w+\\.\\w+\\.alpha$", 0.07, 0.01, 0.2, 1) |>
  gadgetutils::g3_init_guess("^\\w+\\.\\w+\\.l50$", estimate_l50, estimate_l50 * (1 - 0.25), estimate_l50 * (1 + 0.25), 1) |>
  gadgetutils::g3_init_guess("\\.bbin$", 100, 1e-05, 1000, 1) |>
  identity()

# Add bounds penalty for upper/lower bounds
actions <- c(actions, list(g3l_bounds_penalty(params.in)))
model_code <- g3_to_tmb(actions)
)')}

mw_g3_code_run <- function (spec) {
    comp_names <- function (tbl) {
        unname(unlist(lapply(seq_len(nrow(tbl)), function (i) {
            r <- as.list(tbl[i,])
            out <- vapply(c('dist', 'ldist', 'aldist'), function (dist_type) {
                if (!(dist_type %in% names(r))) return("")
                if (r[[dist_type]] == "none") return("")
                return(paste(dist_type, r$name, sep = "_"))
            }, character(1))
            out[nzchar(out)]
        })))
    }
    fleet_lcomp <- comp_names(spec$fleet)
    abund_lcomp <- comp_names(spec$abund)

grouping <- list(fleet = spec$fleet$name, abund = spec$abund$name)
    template_str(r'(
# Optimise model ################################
obj.fn <- gadget3::g3_tmb_adfun(model_code, params.in)

params.out <- gadgetutils::g3_iterative(getwd(),
    wgts = "WGTS",
    model = model_code,
    params.in = params.in,
    grouping = list(
        fleet = ${deparse1(fleet_lcomp)},
        abund = ${deparse1(abund_lcomp)}),
    method = "BFGS",
    control = list(maxit = 100, reltol = 1e-10),
    use_parscale = TRUE,
    shortcut = FALSE,
    cv_floor = 0.05,
    resume_final = FALSE)

# Generate detailed report ######################
fit <- gadgetutils::g3_fit(model_code, params.out)
gadgetplots::gadget_plots(fit, "figs", file_type = "html")
utils::browseURL("figs/model_output_figures.html")
)')}

mw_g3_script <- function (
        spec,
        xlsx = "",
        compile = FALSE,
        run = FALSE) {
    stopifnot(is.list(spec) || is.environment(spec))
    stopifnot(length(intersect(names(spec), c("abund", "area", "fleet", "stock", "time"))) == 5)
 
    # Run fn(row, ...) for each row in tbl
    row_apply <- function (tbl, fn, ...) vapply(
        seq_len(nrow(tbl)),
        function (i) fn(as.list(tbl[i,, drop = FALSE]), ...),
        character(1))

    # Check any supplied data
    for (dat in setdiff(names(spec), c("abund", "area", "fleet", "stock", "time"))) {
        if ('number' %in% names(spec[[dat]])) {
            numcol <- spec[[dat]]$number
        } else if ('weight' %in% names(spec[[dat]])) {
            numcol <- spec[[dat]]$weight
        } else {
            next
        }
        numcol <- suppressWarnings(as.numeric(numcol))
        if (!any(is.finite(numcol))) stop("Invalid / missing data for ", dat)
    }

    paste(c(
        mw_g3_code_header(spec, xlsx, compile = compile, run = run),
        mw_g3_code_area(spec),
        row_apply(spec$time, mw_g3_code_time, spec),
        row_apply(spec$stock, mw_g3_code_stock, spec, xlsx),
        row_apply(spec$fleet, mw_g3_code_fleet, spec, xlsx),
        row_apply(spec$abund, mw_g3_code_abund, spec, xlsx),
        (if (compile) mw_g3_code_compile(spec, xlsx) else ""),
        (if (run) mw_g3_code_run(spec) else ""),
        ""), collapse = "\n")
}
