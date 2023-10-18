source('mw_gadget3.R') ;
eval(parse(text = mw_g3_script(
    sapply(
        c("abund", "area", "fleet", "stock", "time"),
        function (n) readxl::read_excel('anch.xlsx', n)),
    xlsx = 'anch.xlsx',
    compile = TRUE,
    run = TRUE)))
file.copy("figs/model_output_figures.html", "www/demo_output_figures.html")

knitr::pandoc('user-guide.md', 'html')
