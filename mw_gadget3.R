template_str <- function (s) {
    stringr::str_interp(s, parent.frame(1))
}
escape_sym <- Vectorize(function (s) deparse1(as.symbol(s), backtick = TRUE))

mw_g3_code_header <- function (spec) {
    template_str(r'(
library(gadget3)

actions <- list()
)')}

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

mw_g3_code_stock <- function (r, spec) {
    stock_sym <- escape_sym(r$name)
    area_names <- spec$area$name
    template_str(r'(
# Create stock definition for ${r$name} ####################
${stock_sym} <- g3_stock(${deparse1(r$name)}, seq(${deparse1(r$lg_min)}, ${deparse1(r$lg_max)}, ${deparse(r$lg_size)})) |>
  g3s_livesonareas(area_names[${deparse1(area_names)}]) |>
  g3s_age(${deparse1(as.integer(r$lg_min))}, ${deparse1(as.integer(r$lg_max))})

actions_${stock_sym} <- list(
  g3a_growmature(${stock_sym}, g3a_grow_impl_bbinom(
    maxlengthgroupgrowth = 5L)),
  g3a_naturalmortality(${stock_sym}),
  g3a_initialconditions_normalparam(${stock_sym}),
  g3a_renewal_normalparam(${stock_sym},
    run_f = quote( ${if (r$renewal_step == 0) "" else paste0("cur_step == ", r$renewal_step, " && ")}age == stock__minage && !cur_year_projection )),
  g3a_age(${stock_sym}),
  NULL)

actions_likelihood_${stock_sym} <- list(
  g3l_understocking(list(${stock_sym}), weight = 1e+08, nll_breakdown = TRUE),
  NULL)

actions <- c(actions, actions_${stock_sym}, actions_likelihood_${stock_sym})
)')}

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
    fleets = ${deparse1(fleet_list, backtick = TRUE)},
    stocks = ${deparse1(stock_list, backtick = TRUE)},
    function_f = g3l_distribution_${if (is_abundance) "surveyindices_log" else "sumofsquares"}(),
    area_group = area_names,
    report = TRUE,
    nll_breakdown = TRUE),)')}

mw_g3_code_fleet <- function (r, spec) {
    fleet_sym <- escape_sym(r$name)
    area_names <- spec$area$name
    stock_list <- lapply(spec$stock$name, as.symbol)
    data_name <- paste("landings", r$name, sep = "_")
    data_sym <- escape_sym(data_name)

    template_str(r'(
# Create fleet definition for ${r$name} ####################
${fleet_sym} <- g3_fleet(${deparse1(r$name)}) |> g3s_livesonareas(area_names[${deparse1(area_names)}])
actions_${fleet_sym} <- list(
  g3a_predate_fleet(
    ${fleet_sym},
    ${deparse1(stock_list, backtick = TRUE)},
    suitabilities = g3_suitability_exponentiall50(
        g3_parameterized("${fleet_sym}.alpha", by_stock = TRUE),
        g3_parameterized("${fleet_sym}.l50", by_stock = TRUE)),
    catchability_f = g3a_predate_catchability_numberfleet(
      g3_timeareadata(${deparse1(data_name)}, ${data_sym}, "number", areas = area_names))),
  NULL)
actions_likelihood_${fleet_sym} <- list(${mw_g3_code_likelihood_dist("dist", r, spec)}${mw_g3_code_likelihood_dist("ldist", r, spec)}${mw_g3_code_likelihood_dist("aldist", r, spec)}
  NULL)

actions <- c(actions, actions_${fleet_sym}, actions_likelihood_${fleet_sym})
)')}

mw_g3_code_abund <- function (r, spec) {
    fleet_sym <- escape_sym(r$name)
    stock_list <- lapply(spec$stock$name, as.symbol)
    # TODO: Step active

    template_str(r'(
# Create abundance index for ${r$name} ####################
actions_${fleet_sym} <- list(
  NULL)
actions_likelihood_${fleet_sym} <- list(
${mw_g3_code_likelihood_dist("dist", r, spec)}${mw_g3_code_likelihood_dist("ldist", r, spec)}${mw_g3_code_likelihood_dist("aldist", r, spec)}
  NULL)

actions <- c(actions, actions_${fleet_sym}, actions_likelihood_${fleet_sym})
)')}

mw_g3_code_compile <- function (spec) {
    template_str(r'(
# Create model objective function ####################

model_code <- g3_to_tmb(actions)
params.in <- attr(model_code, "parameter_template")
)')}

mw_g3_code_run <- function (spec) {
    template_str(r'(
# Examples for running model ####################
obj.fn <- gadget3::g3_tmb_adfun(model_code, params.in)
obj.fn$report()$anch__num

fit <- gadgetutils::g3_fit(model_code, params.in)
# gadgetplots::plot_ldist(fit)
)')}

mw_g3_script <- function (
        spec,
        compile = FALSE,
        run = FALSE) {
    stopifnot(is.list(spec))
    stopifnot(identical(sort(names(spec)), c("abund", "area", "fleet", "stock", "time")))
 
    # Run fn(row, ...) for each row in tbl
    row_apply <- function (tbl, fn, ...) vapply(
        seq_len(nrow(tbl)),
        function (i) fn(as.list(tbl[i,, drop = FALSE]), ...),
        character(1))

    paste(c(
        mw_g3_code_header(spec),
        mw_g3_code_area(spec),
        row_apply(spec$time, mw_g3_code_time, spec),
        row_apply(spec$stock, mw_g3_code_stock, spec),
        row_apply(spec$fleet, mw_g3_code_fleet, spec),
        row_apply(spec$abund, mw_g3_code_abund, spec),
        (if (compile) mw_g3_code_compile(spec) else ""),
        (if (run) mw_g3_code_run(spec) else ""),
        ""), collapse = "\n")
}
