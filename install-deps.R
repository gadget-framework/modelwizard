withCallingHandlers({
    install.packages('remotes')
    install.packages('here')

    install.packages("shiny")
    install.packages("shinycssloaders")
    remotes::install_github("shuttlethread/hodfr")

    install.packages("readxl")
    install.packages("writexl")
    install.packages("stringr")

    remotes::install_github("gadget-framework/gadget3", "master")  # TODO: New release
    remotes::install_github("gadget-framework/gadgetutils")
    remotes::install_github("gadget-framework/gadgetplots")

    install.packages("r4ss")
    install.packages("reshape2")
}, warning = stop)

# Develpoment dependencies
withCallingHandlers({
    install.packages('unittest')
}, warning = stop)
