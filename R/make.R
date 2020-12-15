##################################################
#
# Data and Code for NCS - SDG analysis
#
##################################################

## ----- clean workspace
rm(list = ls())

## ----- install/update packages
devtools::install_deps()

## ----- load functions in the compendium
devtools::load_all()

  # ---- in case of any problem with the installation of the package GGally
  devtools::install_github("ggobi/ggally")



## ----- load rawdata and icons
raw_dat <- NCSSDGproj::read_matrix()
pathSDG <- NCSSDGproj::load_SDG_icon()
pathNCS <- NCSSDGproj::load_NCS_icon()

## ----- produce FIGURE 1

  # ---- format icon
  icon_SDG <- NCSSDGproj::format_icons(pathSDG, icon_SDG = TRUE)
  icon_NCS <- NCSSDGproj::format_icons(pathNCS, icon_SDG = FALSE)
  
  # ---- format data
  data_long <- NCSSDGproj::matrix_to_longDF(matrix01 = raw_dat)
  SDG_matrix <- NCSSDGproj::matrix_SDG(data_long = data_long)
  

  