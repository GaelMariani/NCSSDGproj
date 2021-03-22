####################################################################################
#                                                                                  #
# Data and Code for NCS - SDG analysis with the new scoring system (from -2 to +2) #
#                                                                                  #
####################################################################################

### ----- clean workspace
rm(list = ls())

### ----- install devtools
install.packages("devtools")

### ----- install/update packages (1 if you want to install all packages)
devtools::install_deps()

### ----- load functions in the compendium
devtools::load_all()

## ---- in case of any problem with the installation of the package GGally try
devtools::install_github("ggobi/ggally")


### ----- load rawdata and icons
sheets <- NCSSDGproj::read_all_sheets()
pathSDG <- NCSSDGproj::load_SDG_icon()
pathNCS <- NCSSDGproj::load_NCS_icon()

##################################################
#
### ----- produce FIGURE 2 - Relationships between NCS implementation and SDG achievement
#
##################################################

## ---- format icon
icon_SDG <- NCSSDGproj::format_icons(pathSDG, icon_SDG = TRUE)
icon_NCS <- NCSSDGproj::format_icons(pathNCS, icon_SDG = FALSE)

## ---- format data
matrix_all <- NCSSDGproj::sheets_to_matrix(sheets_list = sheets)

  # --- positive matrix
  data_pos <- NCSSDGproj::matrix_to_longDF(matrix01 = matrix_all[["score_pos"]])

  # --- negative matrix 
  data_neg <- NCSSDGproj::matrix_to_longDF(matrix01 = matrix_all[["score_neg"]])

  # --- net matrix
  data_net <- NCSSDGproj::matrix_to_longDF(matrix01 = matrix_all[["score_net"]])

