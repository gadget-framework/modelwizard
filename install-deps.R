withCallingHandlers({
    install.packages('remotes')

    install.packages("shiny")
    install.packages("shinycssloaders")
    remotes::install_github("shuttlethread/hodfr")

    remotes::install_github("gadget-framework/gadget3")  # TODO: New release
}, warning = stop)

# Develpoment dependencies
withCallingHandlers({
    install.packages('unittest')
}, warning = stop)
