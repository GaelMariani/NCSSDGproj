#######################################################################
#######################################################################
##                                                                   ##
## Data and Code for NCS - SDG analysis with the new scoring system  ##
##                                                                   ##
#######################################################################
#######################################################################

### ----- STEP 1: clean workspace
rm(list = ls(), envir = .GlobalEnv)  

### ----- STEP 2: install devtools if not already installed
if(!require(devtools)){
  install.packages("devtools")
  library(devtools)
}

### ----- STEP 3: update 'rlang' package if needed
if(packageVersion("rlang") < "0.4.10"){
  install.packages("rlang")
  library(rlang)
}

### ----- STEP 4: install/update packages (1 if you want to install all packages)
devtools::install_deps()

### ----- STEP 5: load functions in the compendium
devtools::load_all()

  ## ---- in case of any problem with the installation of the package GGally try
  devtools::install_github("ggobi/ggally")
  ## ---- Once installed, re-run STEP 4 and STEP 5
  

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
  
  
  ########################################################################
  #                                                                      #
  # NULL MODELS for Modularity + Nestedness + Insurance at the SDG level #
  #                                                                      #
  ######################################################################## 
  source(here::here("make_results", "null_models_SDG_level.R"))
  
  
  #######################################################################################
  #                                                                                     #
  # SENSITIVITY ANALYSIS for Modularity + Nestedness + Insurance - 10% of modified data #
  #                                                                                     #
  #######################################################################################  
  source(here::here("make_results", "sensitivity_analyses_10p.R"))
  
  
  #######################################################################################
  #                                                                                     #
  # SENSITIVITY ANALYSIS for Modularity + Nestedness + Insurance - 20% of modified data #
  #                                                                                     #
  #######################################################################################  
  source(here::here("make_results", "sensitivity_analyses_20p.R"))
  
  
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
  
  
  #####################################################
  #                                                   #
  # produce supp. Fig 1 - Null model positive results #
  #                                                   #
  ##################################################### 
  source(here::here("make_results", "supp_figure_1.R"))
  
  
  ####################################################################
  #                                                                  #
  # produce supp. Fig 2 - Correspondance Analysis for positive links #
  #                                                                  #
  ####################################################################
  source(here::here("make_results", "supp_figure_2.R"))
  
  
  ####################################################################
  #                                                                  #
  # produce supp. Fig 3 - Correspondance Analysis for negative links #
  #                                                                  #
  ####################################################################
  source(here::here("make_results", "supp_figure_3.R"))
  
  
  #####################################################
  #                                                   #
  # produce supp. Fig 4 - Null model negative results #
  #                                                   #
  ##################################################### 
  source(here::here("make_results", "supp_figure_4.R"))
  
  
  ########################################################
  #                                                      #
  # produce supp. Fig 5 and 6 - sensitivity analyses 10p #
  #                                                      #
  ######################################################## 
  source(here::here("make_results", "supp_figure_5_6.R"))
  
  
  #######################
  #                     #
  # produce supp. Fig 7 #
  #                     #
  #######################
  source(here::here("make_results", "supp_figure_7.R"))
  
  
  #######################################################
  #                                                     #
  # produce supp. Fig unused - sensitivity analyses 20p #
  #                                                     #
  ####################################################### 
  source(here::here("make_results", "supp_figure_9_10_unused.R"))
  

  
  