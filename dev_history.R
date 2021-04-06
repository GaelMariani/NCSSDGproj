# Create a research compendium
rrtools::use_compendium("../NCSSDGproj", open = FALSE)


# Create folders
dir.create("rawdata")
dir.create("R")
dir.create("dataR")
dir.create("results")
dir.create("reports")
dir.create("figures")


# Library to be used and put in the DESCRIPTION file
usethis::use_package("dplyr")
usethis::use_package("here")
usethis::use_package("ggplot2")
usethis::use_package("png")
usethis::use_package("grid")
usethis::use_package("tidyr")
usethis::use_package("magrittr")
usethis::use_package("forcats")
usethis::use_package("reshape2")
usethis::use_package("network")
usethis::use_package("GGally")
usethis::use_package("ggimage")
usethis::use_package("bipartite")
usethis::use_package("stats")
usethis::use_package("ade4")
usethis::use_package("cowplots")
usethis::use_package("ggpubr")
usethis::use_package("ggraph")
usethis::use_package("factoextra")
usethis::use_package("FactoMineR")
usethis::use_package("readxl")
usethis::use_package("plyr")
usethis::use_package("ggrepel")
usethis::use_package("openxlsx")
usethis::use_package("forcats")
usethis::use_pipe()

# Create R script 
usethis::use_r("read_data")
usethis::use_r("format_data")
usethis::use_r("analysis")
usethis::use_r("plot_data")
usethis::use_r("utils-pipe")
usethis::use_r("unused_functions")

file.create("make.R")
file.create("make_new_scoring.R")


# Updating NCSSDGproj documentation - NAMESPACE file (to run for each new function)
devtools::document()
