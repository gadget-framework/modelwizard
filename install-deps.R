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
}, warning = stop)

# Develpoment dependencies
withCallingHandlers({
    install.packages('unittest')
}, warning = stop)
