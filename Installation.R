# Checking clean install of `{mosaicCalc}`
install.packages(c("remotes", "knitr", "ggplot2", "rmarkdown"))
# Say "no" when asked "Do you want to install from sources ...?"
install.packages(c())
remotes::install_github("ProjectMOSAIC/mosaicCalc", build_vignettes = TRUE, force=TRUE)

help(package="mosaicCalc")

# The help panel of RStudio will show a page entitled "R-Language Based Calculus ...." Follow the link to
# "User guides, package vignettes, ..." then open the "Beginners' Guide to Calculus with R." Read the document through
# the "Tilde expressions" section, and further if you like.

