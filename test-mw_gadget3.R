if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

source('mw_gadget3.R')

ok_baseline <- function (test_name, output) {
    file_path <- here::here(paste('test-mw_gadget3', test_name, 'baseline', sep = "."))
    if (!file.exists(file_path) || nzchar(Sys.getenv("RESET_BASELINE"))) {
      writeLines(output, con = file_path)
    }
    base_output <- paste(readLines(file_path), collapse = "\n")
    ok(ut_cmp_identical(output, base_output), sprintf("%s: Matches baseline", test_name))
}
xlsx_to_spec <- function (xlsx_path = 'anch.xlsx') {
    spec_tbls <- c('time', 'area', 'stock', 'fleet', 'abund')
    structure(
        lapply(spec_tbls, function(n) readxl::read_xlsx(xlsx_path, n)),
        names = spec_tbls)
}

code <- mw_g3_script(list(
    time = data.frame(
        year_min = 1999,
        year_max = 2004,
        steps = c(4),
        stringsAsFactors = FALSE),
    area = data.frame(
        name = c('IV4', 'IV5'),
        stringsAsFactors = FALSE),
    stock = data.frame(
        name = c("ling"),
        lg_min = 30, lg_max = 50, lg_size = 10,
        age_min = 3, age_max = 5,
        renewal_step = 0,
        stringsAsFactors = FALSE),
    fleet = data.frame(
        name = c("comm", "surv"),
        step_active = 0,
        ldist = c("weight", "none"),
        aldist = c("none", "weight"),
        landings = "weight",
        stringsAsFactors = FALSE),
    abund = data.frame(
        name = c("acoustic"),
        step_active = 0,
        dist = c("weight"),
        stringsAsFactors = FALSE)), compile = TRUE)
#TODO: Need data to eval: eval(parse(text = code))
ok_baseline('script1', code)

code <- mw_g3_script(list(
    time = data.frame(
        year_min = 1999,
        year_max = 2004,
        steps = c(4),
        stringsAsFactors = FALSE),
    area = data.frame(
        name = c('IV4', 'IV5'),
        stringsAsFactors = FALSE),
    stock = data.frame(
        name = c("ling"),
        lg_min = 30, lg_max = 50, lg_size = 10,
        age_min = 3, age_max = 5,
        renewal_step = 1,
        stringsAsFactors = FALSE),
    fleet = data.frame(
        name = c("comm", "surv"),
        step_active = 0,
        ldist = c("weight", "none"),
        aldist = c("none", "weight"),
        landings = "weight",
        stringsAsFactors = FALSE),
    abund = data.frame(
        name = c("acoustic"),
        step_active = 0,
        dist = c("weight"),
        stringsAsFactors = FALSE)), xlsx = "moo.xlsx", compile = TRUE)
#TODO: Need data to eval: eval(parse(text = code))
ok_baseline('script1-withxlsx', code)

code <- mw_g3_script(xlsx_to_spec("anch.xlsx"), xlsx = "anch.xlsx")
ok_baseline('script-anch', code)
eval(parse(text = code))
