# Create a research compendium
rrtools::use_compendium("../NCSSDGproj", open = FALSE)


# Create folders
dir.create("rawdata") # store raw data
dir.create("R") # R scripts 
dir.create("results") # .RData files to store results
dir.create("figures") # save figures
dir.create("make_results") # R scripts to be run in the make.R


# Library to be used and put in the DESCRIPTION file
usethis::use_package("dplyr")
usethis::use_package("here")
usethis::use_package("ggplot2")
usethis::use_package("magrittr")
usethis::use_package("png")
usethis::use_package("grid")
usethis::use_package("tidyr")
usethis::use_package("forcats")
usethis::use_package("reshape2")
usethis::use_package("network")
usethis::use_package("GGally")
usethis::use_package("ggimage")
usethis::use_package("bipartite")
usethis::use_package("stats")
usethis::use_package("cowplot")
usethis::use_package("ggpubr")
usethis::use_package("factoextra")
usethis::use_package("FactoMineR")
usethis::use_package("readxl")
usethis::use_package("plyr")
usethis::use_package("ggrepel")
usethis::use_package("openxlsx")
usethis::use_package("tibble")

usethis::use_pipe()

# Create R script 
usethis::use_r("read_data")
usethis::use_r("format_data")
usethis::use_r("analysis")
usethis::use_r("plot_data")
usethis::use_r("utils-pipe")
usethis::use_r("unused_functions")



file.create("make_results/figure_2.R")
file.create("make_results/figure_3.R")
file.create("make_results/null_models.R")
file.create("make_results/null_models_SDG_level.R")
file.create("make_results/sensitivity_analyses_20p.R")
file.create("make_results/sensitivity_analyses_10p.R")
file.create("make_results/figure_4.R")
file.create("make_results/supp_figure_1.R")
file.create("make_results/supp_figure_2.R")
file.create("make_results/supp_figure_3.R")
file.create("make_results/supp_figure_4.R")



file.create("make.R")


# Updating NCSSDGproj documentation - NAMESPACE file (to run for each new function)
devtools::document()
