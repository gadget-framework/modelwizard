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
    if (!nzchar(sheet_name) || !nzchar(xlsx)) return("")
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
    is_abundance <- endsWith(dist_type, "si")

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
    if ("landings" %in% names(r)) {  # i.e. commercial fleets
        fleet_type <- "commercial"
        data_name <- paste("landings", r$name, sep = "_")
        data_sym <- escape_sym(data_name)
        catchability_fn <- paste0(
            if (identical(r$landings, "weight")) "g3a_predate_catchability_totalfleet" else "g3a_predate_catchability_numberfleet",
            "(g3_timeareadata(",
            deparse1(data_name), ", ",
            data_sym, ", ",
            deparse1(r$landings), ", ",
            "areas = area_names",
            "))" )
    } else {  # i.e. survey fleets
        fleet_type <- "survey"
        data_name <- paste("si", r$name, sep = "_")
        data_sym <- escape_sym(data_name)
        catchability_fn <- "g3a_predate_catchability_totalfleet(1)"
    }

    template_str(r'(
# Create ${fleet_type} fleet definition for ${r$name} ####################
${fleet_sym} <- g3_fleet(${deparse1(r$name)}) |> g3s_livesonareas(area_names[${deparse1(area_names)}])

${mw_g3_code_readxl(data_name, xlsx)}${mw_g3_code_readxl_dist("dist", r, xlsx)}${mw_g3_code_readxl_dist("ldist", r, xlsx)}${mw_g3_code_readxl_dist("aldist", r, xlsx)}
${actions_sym} <- list(
  g3a_predate_fleet(
    ${fleet_sym},
    ${deparse1(stock_list, backtick = TRUE)},
    suitabilities = g3_suitability_exponentiall50(),
    catchability_f = ${catchability_fn} ),
  NULL)
${actions_likelihood_sym} <- list(${mw_g3_code_likelihood_dist("si", r, spec)}${mw_g3_code_likelihood_dist("dist", r, spec)}${mw_g3_code_likelihood_dist("ldist", r, spec)}${mw_g3_code_likelihood_dist("aldist", r, spec)}
  NULL)

actions <- c(actions, ${actions_sym}, ${actions_likelihood_sym})
)')}

mw_g3_code_compile <- function (spec, xlsx) {
    stock_list <- lapply(spec$stock$name, as.symbol)
    template_str(r'(
# Create model objective function ####################

# Turn model into C++ code
model_code <- g3_to_tmb(c(actions, list(
    # Include detailed reporting for all actions
    g3a_report_detail(actions),
    # Add lower/upper bounds from parameters to output likelihood
    g3l_bounds_penalty(actions) )))

# Guess l50 / linf based on stock sizes
estimate_l50 <- gadget3::g3_stock_def(${deparse1(stock_list[[1]], backtick = TRUE)}, "midlen")[[length(gadget3::g3_stock_def(${deparse1(stock_list[[1]], backtick = TRUE)}, "midlen")) / 2]]
estimate_linf <- max(gadget3::g3_stock_def(${deparse1(stock_list[[1]], backtick = TRUE)}, "midlen"))
estimate_t0 <- gadget3::g3_stock_def(${deparse1(stock_list[[1]], backtick = TRUE)}, "minage") - 0.8

attr(model_code, "parameter_template") |>
  # fish.init.scalar & fish.rec.scalar: Overall scalar for recruitment/initial conditions, see g3a_renewal_normalparam()
  g3_init_val("*.rec|init.scalar", 10, lower = 0.001, upper = 200) |>
  # fish.rec.(age): Per-age recriutment scalar, see g3a_renewal_normalparam()
  g3_init_val("*.init.#", 10, lower = 0.001, upper = 200) |>
  # fish.rec.(year): Recruitment level year-on-year, see g3a_renewal_normalparam()
  g3_init_val("*.rec.#", 100, lower = 1e-6, upper = 1000) |>
  # fish.rec.sd: Standard deviation for recruitment, see g3a_renewal_normalparam()
  g3_init_val("*.rec.sd", 5, lower = 4, upper = 20) |>
  # init.F: Offset for initial M, see g3a_renewal_initabund()
  g3_init_val("init.F", 0.5, lower = 0.1, upper = 1) |>

  # fish.M.(age): per-age M for our species, see g3a_naturalmortality()
  g3_init_val("*.M.#", 0.15, lower = 0.001, upper = 1) |>

  # fish.Linf, fish.K, fish.t0: VonB parameters for our species, see g3a_renewal_vonb_t0(), g3a_grow_lengthvbsimple()
  g3_init_val("*.Linf", estimate_linf, spread = 0.2) |>
  g3_init_val("*.K", 0.3, lower = 0.04, upper = 1.2) |>
  g3_init_val("*.t0", estimate_t0, spread = 2) |>

  # fish.walpha, fish.wbeta: Age/weight relationship for initialconditions, renewal, see g3a_renewal_normalparam()
  g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
  g3_init_val("*.wbeta", 3, optimise = FALSE) |>

  # fish.f_surv.alpha, fish.f_surv.l50: Curve/l50 for fishing suitability, see g3_suitability_exponentiall50()
  g3_init_val("*.*.alpha", 0.07, lower = 0.01, upper = 0.2) |>
  g3_init_val("*.*.l50", estimate_l50, spread = 0.25) |>

  # fish.bbin: Beta for beta-binomial distribution for fish growth, see g3a_grow_impl_bbinom()
  g3_init_val("*.bbin", 100, lower = 1e-05, upper = 1000) |>

  # identity() is a do-nothing function, but it lets us finish on a new line
  identity() -> params.in
)')}

mw_g3_code_run <- function (spec) {
    comp_names <- function (tbl) {
        unname(unlist(lapply(seq_len(nrow(tbl)), function (i) {
            r <- as.list(tbl[i,])
            out <- vapply(c('si', 'dist', 'ldist', 'aldist'), function (dist_type) {
                if (!(dist_type %in% names(r))) return("")
                if (r[[dist_type]] == "none") return("")
                return(paste(dist_type, r$name, sep = "_"))
            }, character(1))
            out[nzchar(out)]
        })))
    }
    comm_lcomp <- comp_names(spec$comm)
    surv_lcomp <- comp_names(spec$surv)

grouping <- list(surv = spec$surv$name, comm = spec$comm$name)
    template_str(r'(
# Optimise model ################################
obj.fn <- gadget3::g3_tmb_adfun(model_code, params.in)

params.out <- gadgetutils::g3_iterative(getwd(),
    wgts = "WGTS",
    model = model_code,
    params.in = params.in,
    grouping = list(
        comm = ${deparse1(comm_lcomp)},
        surv = ${deparse1(surv_lcomp)}),
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
    stopifnot(length(intersect(names(spec), c("area", "comm", "surv", "stock", "time"))) == 5)
 
    # Run fn(row, ...) for each row in tbl
    row_apply <- function (tbl, fn, ...) vapply(
        seq_len(nrow(tbl)),
        function (i) fn(as.list(tbl[i,, drop = FALSE]), ...),
        character(1))

    # Check any supplied data
    for (dat in setdiff(names(spec), c("area", "comm", "surv", "stock", "time"))) {
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
        row_apply(spec$comm, mw_g3_code_fleet, spec, xlsx),
        row_apply(spec$surv, mw_g3_code_fleet, spec, xlsx),
        (if (compile) mw_g3_code_compile(spec, xlsx) else ""),
        (if (run) mw_g3_code_run(spec) else ""),
        ""), collapse = "\n")
}
