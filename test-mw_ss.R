if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

source('mw_ss.R')

ok_baseline <- function (test_name, output) {
    file_path <- here::here(paste('test-mw_ss', test_name, 'baseline', sep = "."))
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

code <- mw_ss_script(xlsx_to_spec("anch.xlsx"), xlsx = "anch.xlsx")
ok_baseline('script-anch', code)
eval(parse(text = code))
