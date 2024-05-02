if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

source('mw_gadget3.R')
library(readxl)

source_xlsx <- 'anch.xlsx'
script_out <- 'test-mw_gadget3.anch.baseline'

code <- mw_g3_script(list(
    time = read_excel(source_xlsx, 'time'),
    area = read_excel(source_xlsx, 'area'),
    stock = read_excel(source_xlsx, 'stock'),
    comm = read_excel(source_xlsx, 'comm'),
    surv = read_excel(source_xlsx, 'surv') ), xlsx = source_xlsx, compile = TRUE, run = TRUE)
writeLines(code, con = script_out)
source(script_out, echo = TRUE)
