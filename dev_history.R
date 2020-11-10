# Create a research compendium
rrtools::use_compendium("../NCSSDGproj", open = FALSE)


# Create folders
dir.create("rawdata")
dir.create("R")
dir.create("dataR")
dir.create("results")
dir.create("reports")


# Library to be used put in the DESCRIPTION file
usethis::use_package("dplyr")
usethis::use_package("here")
usethis::use_package("ggplot2")
usethis::use_package("png")
usethis::use_package("grid")
usethis::use_package("tidyr")
usethis::use_package("magrittr")
usethis::use_package("forcats")
usethis::use_package("reshape2")
#usethis::use_package("ggnet")
usethis::use_package("network")
#usethis::use_package("GGally")
usethis::use_package("ggimage")
usethis::use_pipe()


# Create R script 
usethis::use_r("read_data")
usethis::use_r("format_data")
usethis::use_r("analysis")
usethis::use_r("plot_data")
usethis::use_r("utils-pipe")


# Updating NCSSDGproj documentation - NAMESPACE file (to run for each new function)
devtools::document()
