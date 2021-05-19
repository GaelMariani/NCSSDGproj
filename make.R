#######################################################################
#######################################################################
##                                                                   ##
## Data and Code for NCS - SDG analysis with the new scoring system  ##
##                                                                   ##
#######################################################################
#######################################################################

### ----- clean workspace
rm(list = ls(), envir = .GlobalEnv)  

### ----- install devtools
install.packages("devtools")

### ----- install/update packages (1 if you want to install all packages)
devtools::install_deps()

### ----- load functions in the compendium
devtools::load_all()

  ## ---- in case of any problem with the installation of the package GGally try
  devtools::install_github("ggobi/ggally")


  ###################################################################################
  #                                                                                 #
  # produce FIGURE 2 - Relationships between NCS implementation and SDG achievement #
  #                                                                                 #
  ###################################################################################
  source(here::here("make_results", "figure_2.R"))
  
  
  #######################################################
  #                                                     #
  # NULL MODELS for Modularity + Nestedness + Insurance #
  #                                                     #
  ####################################################### 
  source(here::here("make_results", "null_models.R"))
  
  
  ################################################################
  #                                                              #
  # SENSITIVITY ANALYSIS for Modularity + Nestedness + Insurance #
  #                                                              #
  ################################################################  
  source(here::here("make_results", "sensitivity_analyses.R"))
  
  
  ############################################################
  #                                                          #
  # produce FIGURE 3 - Target's insurance for positive links #
  #                                                          #
  ############################################################    
  source(here::here("make_results", "figure_3.R"))
  
  
  ############################################################
  #                                                          #
  # produce FIGURE 4 - Target's insurance for negative links #
  #                                                          #
  ############################################################ 
  source(here::here("make_results", "figure_4.R"))
  
  
  ####################################################################
  #                                                                  #
  # produce supp. Fig 1 - Correspondance Analysis for positive links #
  #                                                                  #
  ####################################################################
  source(here::here("make_results", "supp_figure_1.R"))
              
  
  ####################################################################
  #                                                                  #
  # produce supp. Fig 2 - Correspondance Analysis for negative links #
  #                                                                  #
  ####################################################################
  source(here::here("make_results", "supp_figure_2.R"))
  
  
  #######################################
  #                                     #
  # produce supp. Fig 3 and supp. Fig 4 #
  #                                     #
  ####################################### 
  source(here::here("make_results", "supp_figure_3_4.R"))
  
  
  


  
  
  
  
  
  