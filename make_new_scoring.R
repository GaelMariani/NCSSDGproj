######################################################################################
######################################################################################
##                                                                                  ##
## Data and Code for NCS - SDG analysis with the new scoring system (from -2 to +2) ##
##                                                                                  ##
######################################################################################
######################################################################################

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

### ----- LOAD DATA (rawdata and icons)
sheets  <- NCSSDGproj::read_all_sheets()
pathSDG <- NCSSDGproj::load_SDG_icon()
pathNCS <- NCSSDGproj::load_NCS_icon()


### ----- FORMAT ICONS (NCS and SDG icons)
icon_SDG <- NCSSDGproj::format_icons(pathSDG, icon_SDG = TRUE)
icon_NCS <- NCSSDGproj::format_icons(pathNCS, icon_SDG = FALSE)
  

### ----- FORMAT DATA
matrix_all <- NCSSDGproj::sheets_to_matrix(sheets_list = sheets)

SDG_network <- lapply(1:length(matrix_all), 
                      function(i){
                        
                        # -- matrix in long format
                        data_long   <- NCSSDGproj::matrix_to_longDF(matrix01 = matrix_all[[i]])
                        
                        # -- weighted contingency matrix of SDG
                        SDG_matrix  <- NCSSDGproj::matrix_SDG(data_long = data_long)
                        
                        # -- create a network object
                        SDG_network <- NCSSDGproj::matrix_to_network(matrix = SDG_matrix,
                                                                     mode1  = "P",
                                                                     mode2  = "A")
                        
                        # -- percentage of target achieved
                        data_pourc  <- NCSSDGproj::perc_SDG(data_long = data_long)
                        
                        return(list(data_long = data_long, matrix = SDG_matrix, network = SDG_network, data_pourc = data_pourc))
                        
                      })
names(SDG_network) <- names(matrix_all)


### ----- PLOT DATA

  ## ---- Plot panel A - the bipartite network
  NCSSDGproj::plot_network(network_obj = SDG_network[["score_cumulate"]][["network"]],
                           matrix      = SDG_network[["score_cumulate"]][["matrix"]],
                           icon_SDG    = icon_SDG,
                           icon_NCS    = icon_NCS,
                           nodes_col   = c(rep("#228B22", 4), rep("#5EA9A2", 4), rep("#1134A6", 3)),
                           save        = TRUE)
    
  ## ---- Plot panel B - the barplot
  NCSSDGproj::barplot_perc_achieve(SDG_network = SDG_network, 
                                   color       = c("#1134A6", "#5EA9A2",  "#228B22", "#1134A6", "#5EA9A2",  "#228B22"), # Mar, Coast, Ter, Mar_neg, Coast_neg, Ter_neg
                                   save        = TRUE)
  
  ## ---- Plot legend for the two plots
  NCSSDGproj::barplot_legend(data_plot = SDG_network[["score_pos"]][["data_pourc"]], 
                             color     = c("#1134A6", "#5EA9A2", "#228B22"))
    
  ## ---- Bind fig 1A with fig 1B
  NCSSDGproj::Figure2(save = TRUE)
  
  
#####################################################################
#                                                                   #
# produce FIGURE 3 - Complementarity of NCS in target's achievement #
#                                                                   #
#####################################################################
rm(list = ls(), envir = .GlobalEnv)
  
  
### ----- LOAD DATA
  
  ## ---- Data of links between NCS and SDG
  sheets  <- NCSSDGproj::read_all_sheets()

### ----- FORMAT DATA

  ## ---- From sheets to df
  matrix_all <- NCSSDGproj::sheets_to_matrix(sheets_list = sheets)
  
  ## ---- From dataframes to contingency matrices 
  
    # --- binary data
    matrix_conting_bin <- lapply(matrix_all, NCSSDGproj::contingency_mat_targets, binary = TRUE)
    
    # --- 0-1-2 score
    matrix_conting <- lapply(matrix_all, NCSSDGproj::contingency_mat_targets, binary = FALSE)
  
  ## ---- Informations on the NCS
  info_NCS <- NCSSDGproj::NCS_info(matrix_cont = matrix_conting_bin[[1]])
  
  ## ---- Table of the most contributing targets
  axis2_targ <- NCSSDGproj::SDG_contrib_tbl() 
  
### ----- ANALYSIS  
  
  ## ---- Binary analysis (CHi2 distance and correspondance analysis)
  
    # ---- Correspondance analysis for POSITIVE links with the contribution of each target and NCS to the variance of each axis
    contri_CA_pos <- NCSSDGproj::CA_contri_vars(matrix_cont  = matrix_conting_bin[["score_pos"]],
                                                axis2_targ   = axis2_targ,
                                                colNCS_ter   = "#228B22",
                                                colNCS_coast = "#5EA9A2",
                                                colNCS_mar   = "#1134A6")
    
    # ---- Correspondance analysis for NEGATIVE links with the contribution of each target and NCS to the variance of each axis
    contri_CA_neg <- NCSSDGproj::CA_contri_vars(matrix_cont  = matrix_conting_bin[["score_neg"]],
                                                axis2_targ   = axis2_targ,
                                                colNCS_ter   = "#228B22",
                                                colNCS_coast = "#5EA9A2",
                                                colNCS_mar   = "#1134A6")
    
  ## ---- Non binary analysis (Bray-Curtis distance and PCoA)
    
    
    
    
### PLOT DATA 
  
  ## ---- Figure with all panels for POSITIVE links
  NCSSDGproj::Figure3(data           = contri_CA_pos[["CorresAna"]],
                      targ_contrib12 = contri_CA_pos[["col_contrib"]][["axe12"]],
                      data_arrow     = contri_CA_pos[["data_arrow"]],
                      colNCS_ter     = "#228B22", 
                      colNCS_coast   = "#5EA9A2",
                      colNCS_mar     = "#1134A6",
                      save           = TRUE,
                      name           = "Figure3_pos")
  
  ## ---- Figure with all panels for NEGATIVE links
  NCSSDGproj::Figure3(data           = contri_CA_neg[["CorresAna"]],
                      targ_contrib12 = contri_CA_neg[["col_contrib"]][["axe12"]],
                      data_arrow     = contri_CA_neg[["data_arrow"]],
                      colNCS_ter     = "#228B22", 
                      colNCS_coast   = "#5EA9A2",
                      colNCS_mar     = "#1134A6",
                      save           = TRUE,
                      name           = "Figure3_neg")
  
  

#######################################################
#                                                     #
# NULL MODELS for Modularity + Nestedness + Insurance #
#                                                     #
#######################################################  
rm(list = ls(), envir = .GlobalEnv)
  
### ----- LOAD DATA
  
  ## ---- Data of links between NCS and SDG
  sheets  <- NCSSDGproj::read_all_sheets()
  
### ----- FORMAT DATA
  
  ## ---- From sheets to df
  matrix_all <- NCSSDGproj::sheets_to_matrix(sheets_list = sheets, binary = TRUE)
  
  ## ---- From dataframes to contingency matrices 
  matrix_conting_bin <- lapply(matrix_all, NCSSDGproj::contingency_mat_targets, binary = TRUE)
    

### ----- ANALYSIS
  
  ## ---- Modularity and Nestedness - NMalgo = "quasiswap" to conserve marginal sums
  
    # --- POSITIVE scores
    Nest_mod_res_pos <- NCSSDGproj::NullModels(matrix01        = matrix_conting_bin[["score_pos"]], 
                                               rawdata         = matrix_all[["score_pos"]], 
                                               NMalgo          = "quasiswap", 
                                               NESTmethod      = "NODF",
                                               Nrun            = 1, 
                                               Nsim            = 99, # Nsim = 1000 in the paper - TAKES TIME TO RUN
                                               TargetInsurance = FALSE,
                                               save            = TRUE,
                                               name            = "Nest_Modu_res_pos")
    
    # --- NEGATIVE scores
    Nest_mod_res_neg <- NCSSDGproj::NullModels(matrix01        = matrix_conting_bin[["score_neg"]], 
                                               rawdata         = matrix_all[["score_neg"]], 
                                               NMalgo          = "quasiswap", 
                                               NESTmethod      = "NODF",
                                               Nrun            = 1, 
                                               Nsim            = 99, # Nsim = 1000 in the paper - TAKES TIME TO RUN
                                               TargetInsurance = FALSE,
                                               save            = TRUE,
                                               name            = "Nest_Modu_res_neg")
    
  ## ---- Insurance - NMalgo = "r00" to not conserve marginal sums
    
    # --- POSITIVE scores
    TUI_TOI_res_pos <- NCSSDGproj::NullModels(matrix01        = matrix_conting_bin[["score_pos"]], 
                                              rawdata         = matrix_all[["score_pos"]], 
                                              NMalgo          = "r00",
                                              Nsim            = 999, # Nsim = 1000 in the paper - TAKES TIME TO RUN
                                              TargetInsurance = TRUE,
                                              save            = TRUE,
                                              name            = "TUI_TOI_res_pos")
    
    
    # --- NEGATIVE scores
    TUI_TOI_res_neg <- NCSSDGproj::NullModels(matrix01        = matrix_conting_bin[["score_neg"]], 
                                              rawdata         = matrix_all[["score_neg"]], 
                                              NMalgo          = "r00", 
                                              Nsim            = 999, # Nsim = 1000 in the paper - TAKES TIME TO RUN
                                              TargetInsurance = TRUE,
                                              save            = TRUE,
                                              name            = "TUI_TOI_res_neg")
    
    
################################################################
#                                                              #
# SENSITIVITY ANALYSIS for Modularity + Nestedness + Insurance #
#                                                              #
################################################################    
rm(list = ls(), envir = .GlobalEnv)
    
### ----- LOAD DATA
    
  ## ---- Data of links between NCS and SDG
  sheets  <- NCSSDGproj::read_all_sheets()
  
  ## Observed modularity and nestedness values
  obs_metric <- NCSSDGproj::load_metric_obs()
    
### ----- FORMAT DATA
    
  ## ---- From sheets to df
  matrix_all <- NCSSDGproj::sheets_to_matrix(sheets_list = sheets, binary = FALSE) 
  
  ## ---- Randomly turn x% of values and do it 999 times for positive and negative scores
  matrices_modif_0.05 <- list(# Positive scores
                              "score_pos" = replicate(n        = 99,
                                                      simplify = FALSE,
                                                      expr     =  NCSSDGproj::turn_values_randomly(data_links = matrix_all[["score_pos"]],
                                                                                                   percentage = 0.05, 
                                                                                                   binary     = TRUE)),
                              
                              # Negative scores
                              "score_neg" = replicate(n        = 99, 
                                                      simplify = FALSE,
                                                      expr     = NCSSDGproj::turn_values_randomly(data_links = matrix_all[["score_neg"]],
                                                                                                  percentage = 0.05, 
                                                                                                  binary     = TRUE)))
  
  matrices_modif_0.1 <- list(# Positive scores
                             "score_pos" = replicate(n        = 99,
                                                     simplify = FALSE,
                                                     expr     =  NCSSDGproj::turn_values_randomly(data_links = matrix_all[["score_pos"]],
                                                                                                  percentage = 0.1, 
                                                                                                  binary     = TRUE)),
                            
                             # Negative scores
                             "score_neg" = replicate(n        = 99, 
                                                     simplify = FALSE,
                                                     expr     = NCSSDGproj::turn_values_randomly(data_links = matrix_all[["score_neg"]],
                                                                                                 percentage = 0.1, 
                                                                                                 binary     = TRUE)))
  
  ## ---- From dataframes to contingency matrices
  matrix_conting_bin_pos0.05 <- lapply(matrices_modif_0.05[["score_pos"]], NCSSDGproj::contingency_mat_targets, binary = TRUE)
  matrix_conting_bin_pos0.1 <- lapply(matrices_modif_0.1[["score_pos"]], NCSSDGproj::contingency_mat_targets, binary = TRUE)
  
  
### ----- ANALYSIS
sensitivity_analysis0.05 <- NCSSDGproj::sensitivity_analysis(matrix_rep = matrix_conting_bin_pos0.05,
                                                             obs_values = obs_metric,
                                                             Nrun       = 1,
                                                             save       = TRUE,
                                                             name       = "sensitivity_analysis_res0.05")

sensitivity_analysis0.1 <- NCSSDGproj::sensitivity_analysis(matrix_rep = matrix_conting_bin_pos0.1,
                                                            obs_values = obs_metric,
                                                            Nrun       = 1,
                                                            save       = TRUE,
                                                            name       = "sensitivity_analysis_res0.1")


    
#########################################
#                                       #
# produce FIGURE 4 - Target's insurance #
#                                       #
#########################################      
rm(list = ls(), envir = .GlobalEnv)    

### ----- LOAD DATA
    
  ## ---- Data of links between NCS and SDG
  sheets  <- NCSSDGproj::read_all_sheets()
  
  ## ---- SDG icons
  pathSDG <- NCSSDGproj::load_SDG_icon()

### ----- FORMAT DATA
  
  ## ---- From sheets to df
  matrix_all <- NCSSDGproj::sheets_to_matrix(sheets_list = sheets, binary = TRUE)
  
  ## ---- From dataframes to contingency matrices 
  matrix_conting_bin <- lapply(matrix_all, NCSSDGproj::contingency_mat_targets, binary = TRUE)
  
  ## ---- Format data into a long data frame
  data_long <- lapply(matrix_all, NCSSDGproj::matrix_to_longDF)
  
  ## ---- Informations on NCSs
  NCS_info <- NCSSDGproj::NCS_info(matrix_cont = matrix_conting_bin[["score_pos"]])
  
  ## ---- Informations on SDGs
  SDG_info <- NCSSDGproj::SDG_infos(matrix_cont = matrix_conting_bin[["score_pos"]])
  
  ## ---- SDG icons
  icon_SDG <- NCSSDGproj::format_icons(pathSDG, icon_SDG = TRUE)
  
  
  
### ----- ANALYSIS
  
  ## ---- Compute target's insurance
  data_Insurance <- NCSSDGproj::Insurance_data2plot(matrix01 = matrix_conting_bin[["score_pos"]], 
                                                    Ntarget  = ncol(matrix_conting_bin[["score_pos"]])) 
  
  ## ---- Format data to plot
  data_circu <- NCSSDGproj::circular_data_Insurance(data_Insurance = data_Insurance, 
                                                    data_long = data_long[["score_pos"]], 
                                                    SDG_info = SDG_info, 
                                                    NCS_info = NCS_info) # format data with polar coordinates
  
### ----- PLOT DATA
NCSSDGproj::circular_plot_Insurance(data         = data_circu[[1]], 
                                    label_data   = data_circu[[2]],
                                    base_data    = data_circu[[3]],
                                    grid_data    = data_circu[[4]],
                                    SDG_info     = SDG_info,
                                    colNCS_ter   = "#228B22", 
                                    colNCS_coast = "#5EA9A2",
                                    colNCS_mar   = "#1134A6",
                                    iconSDG      = icon_SDG,
                                    save         = TRUE,
                                    name         = "Figure4_pos")   


    
    
    
    
    
     

complementarity_net <- bipartite::networklevel(web   = test[[1]], 
                                               index = c("niche overlap", "functional complementarity"),
                                               level = "lower")
  
complementarity_grp <- bipartite::grouplevel(web   = test[[1]], 
                                             index = c("niche overlap", "functional complementarity"),
                                             level = "lower")

bipartite::grouplevel(web   = test[[1]][1:2,], 
                      index = c("niche overlap", "functional complementarity"),
                      level = "lower")

overlap_pos <- spaa::niche.overlap(mat    = t(test[[1]]),
                                   method = "levins")
overlap_pos <- spaa::niche.overlap(mat    = t(test[[1]][1:4]),
                                   method = "levins")

spaa::niche.overlap()


mean(overlap_pos)

overlap_neg <- spaa::niche.overlap(mat    = t(test[[2]]),
                                   method = "levins")
mean(overlap_neg)

