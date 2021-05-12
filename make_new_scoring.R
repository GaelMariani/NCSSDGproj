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
matrix_all <- NCSSDGproj::sheets_to_matrix(sheets_list = sheets, binary = TRUE)

  ## Positive links
  data_long_pos <- NCSSDGproj::matrix_to_longDF(matrix01 = matrix_all[["score_pos"]])
  SDG_matrix_pos  <- t(NCSSDGproj::matrix_SDG(data_long = data_long_pos))
  SDG_network_pos <- NCSSDGproj::matrix_to_network(matrix = SDG_matrix_pos,
                                                   mode1  = "P",
                                                   mode2  = "A",
                                                   neg    = FALSE)
  data_pourc_pos  <- NCSSDGproj::perc_SDG(data_long = data_long_pos)


  ## Negative links
  data_long_neg <- NCSSDGproj::matrix_to_longDF(matrix01 = matrix_all[["score_neg"]])
  SDG_matrix_neg  <- NCSSDGproj::matrix_SDG(data_long = data_long_neg)
  SDG_network_neg <- NCSSDGproj::matrix_to_network(matrix = SDG_matrix_neg,
                                                   mode1  = "P",
                                                   mode2  = "A",
                                                   neg    = TRUE)
  data_pourc_neg  <- NCSSDGproj::perc_SDG(data_long = data_long_neg)
  
  ## merge data
  SDG_network <- list("score_pos" = list(data_long = data_long_pos, matrix = SDG_matrix_pos, network = SDG_network_pos, data_pourc = data_pourc_pos),
                      "score_neg" = list(data_long = data_long_neg, matrix = SDG_matrix_neg, network = SDG_network_neg, data_pourc = data_pourc_neg))

# SDG_network <- lapply(1:length(matrix_all), 
#                       function(i){
#                         
#                         # -- matrix in long format
#                         data_long   <- NCSSDGproj::matrix_to_longDF(matrix01 = matrix_all[[i]])
#                         
#                         # -- weighted contingency matrix of SDG
#                         SDG_matrix  <- NCSSDGproj::matrix_SDG(data_long = data_long)
#                         
#                         # -- create a network object
#                         SDG_network <- NCSSDGproj::matrix_to_network(matrix = SDG_matrix,
#                                                                      mode1  = "P",
#                                                                      mode2  = "A",
#                                                                      pos = TRUE)
#                         
#                         # -- percentage of target achieved
#                         data_pourc  <- NCSSDGproj::perc_SDG(data_long = data_long)
#                         
#                         return(list(data_long = data_long, matrix = SDG_matrix, network = SDG_network, data_pourc = data_pourc))
#                         
#                       })
# names(SDG_network) <- names(matrix_all)

### ----- PLOT DATA

  ## ---- Plot panel A - the bipartite positive network
  NCSSDGproj::plot_network_pos(network_obj = SDG_network[["score_pos"]][["network"]],
                               matrix      = SDG_network[["score_pos"]][["matrix"]],
                               icon_SDG    = icon_SDG,
                               icon_NCS    = icon_NCS,
                               nodes_col   = c(rep("#228B22", 4), rep("#5EA9A2", 4), rep("#1134A6", 3)),
                               save        = TRUE,
                               name        = "network_SDG_NCS_pos_V3")
  
  ## ---- Plot panel C - the bipartite negative network
  NCSSDGproj::plot_network_neg(network_obj = SDG_network[["score_neg"]][["network"]],
                               matrix      = SDG_network[["score_neg"]][["matrix"]],
                               icon_SDG    = icon_SDG,
                               icon_NCS    = icon_NCS,
                               nodes_col   = c(rep("#228B22", 4), rep("#5EA9A2", 4), rep("#1134A6", 3)),
                               save        = TRUE,
                               name        = "network_SDG_NCS_neg_V4")

    
  ## ---- Plot panel B - the barplot
  NCSSDGproj::barplot_perc_achieve(SDG_network = SDG_network, 
                                   color       = c("#1134A6", "#5EA9A2",  "#228B22", "#1134A6", "#5EA9A2",  "#228B22"), # Mar, Coast, Ter, Mar_neg, Coast_neg, Ter_neg
                                   save        = TRUE,
                                   name        = "barplot_perc_achieve_V7")
  
  ## ---- Plot legend for the two plots
  NCSSDGproj::barplot_legend(data_plot = SDG_network[["score_pos"]][["data_pourc"]], 
                             color     = c("#1134A6", "#5EA9A2", "#228B22"))
    
  ## ---- Bind fig 1A with fig 1B
  NCSSDGproj::Figure2(save = TRUE,
                      name = "Figure2_V9")
  
  
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
  matrix_all <- NCSSDGproj::sheets_to_matrix(sheets_list = sheets, binary = TRUE)
  
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
                      name           = "Figure3_pos_V2")
  
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
                                               Nrun            = 1, # Nrun = 10 in the paper
                                               Nsim            = 99, # Nsim = 999 in the paper - TAKES TIME TO RUN
                                               TargetInsurance = FALSE,
                                               save            = TRUE,
                                               name            = "Nest_Modu_res_pos")
    
    # --- NEGATIVE scores
    Nest_mod_res_neg <- NCSSDGproj::NullModels(matrix01        = matrix_conting_bin[["score_neg"]], 
                                               rawdata         = matrix_all[["score_neg"]], 
                                               NMalgo          = "quasiswap", 
                                               NESTmethod      = "NODF",
                                               Nrun            = 1, # Nrun = 10 in the paper
                                               Nsim            = 99, # Nsim = 999 in the paper - TAKES TIME TO RUN
                                               TargetInsurance = FALSE,
                                               save            = TRUE,
                                               name            = "Nest_Modu_res_neg")
    
  ## ---- Insurance - NMalgo = "r00" to not conserve marginal sums
    
    # --- POSITIVE scores
    TUI_TOI_res_pos <- NCSSDGproj::NullModels(matrix01        = matrix_conting_bin[["score_pos"]], 
                                              rawdata         = matrix_all[["score_pos"]], 
                                              NMalgo          = "r00",
                                              Nsim            = 999, # Nsim = 999 in the paper - TAKES TIME TO RUN
                                              TargetInsurance = TRUE,
                                              save            = TRUE,
                                              name            = "TUI_TOI_res_pos")
    
    
    # --- NEGATIVE scores
    TUI_TOI_res_neg <- NCSSDGproj::NullModels(matrix01        = matrix_conting_bin[["score_neg"]], 
                                              rawdata         = matrix_all[["score_neg"]], 
                                              NMalgo          = "r00", 
                                              Nsim            = 999, # Nsim = 999 in the paper - TAKES TIME TO RUN
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
                              "score_pos" = replicate(n        = 99, # change by 999 for the paper
                                                      simplify = FALSE,
                                                      expr     = NCSSDGproj::turn_values_randomly(data_links  = matrix_all[["score_pos"]],
                                                                                                   percentage = 0.05, 
                                                                                                   binary     = TRUE)),
                              
                              # Negative scores
                              "score_neg" = replicate(n        = 99, # change by 999 for the paper
                                                      simplify = FALSE,
                                                      expr     = NCSSDGproj::turn_values_randomly(data_links = matrix_all[["score_neg"]],
                                                                                                  percentage = 0.05, 
                                                                                                  binary     = TRUE)))
  
  matrices_modif_0.1 <- list(# Positive scores
                             "score_pos" = replicate(n        = 99, # change by 999 for the paper
                                                     simplify = FALSE,
                                                     expr     =  NCSSDGproj::turn_values_randomly(data_links = matrix_all[["score_pos"]],
                                                                                                  percentage = 0.1, 
                                                                                                  binary     = TRUE)),
                            
                             # Negative scores
                             "score_neg" = replicate(n        = 99, # change by 999 for the paper
                                                     simplify = FALSE,
                                                     expr     = NCSSDGproj::turn_values_randomly(data_links = matrix_all[["score_neg"]],
                                                                                                 percentage = 0.1, 
                                                                                                 binary     = TRUE)))
  
  ## ---- From dataframes to contingency matrices
  matrix_conting_bin_pos0.05 <- lapply(matrices_modif_0.05[["score_pos"]], NCSSDGproj::contingency_mat_targets, binary = TRUE)
  matrix_conting_bin_pos0.1 <- lapply(matrices_modif_0.1[["score_pos"]], NCSSDGproj::contingency_mat_targets, binary = TRUE)
  matrix_conting_bin_neg0.05 <- lapply(matrices_modif_0.05[["score_neg"]], NCSSDGproj::contingency_mat_targets, binary = TRUE)
  matrix_conting_bin_neg0.1 <- lapply(matrices_modif_0.1[["score_neg"]], NCSSDGproj::contingency_mat_targets, binary = TRUE)
  
  
### ----- ANALYSIS
sensitivity_analysis_pos0.05 <- NCSSDGproj::sensitivity_analysis(matrix_rep = matrix_conting_bin_pos0.05,
                                                                 obs_values = obs_metric[["score_pos"]],
                                                                 Nrun       = 1,
                                                                 save       = TRUE,
                                                                 name       = "sensitivity_analysis_res_pos0.05")

sensitivity_analysis_pos0.1 <- NCSSDGproj::sensitivity_analysis(matrix_rep = matrix_conting_bin_pos0.1,
                                                                obs_values = obs_metric[["score_pos"]],
                                                                Nrun       = 1,
                                                                save       = TRUE,
                                                                name       = "sensitivity_analysis_res_pos0.1")


sensitivity_analysis_neg0.05 <- NCSSDGproj::sensitivity_analysis(matrix_rep = matrix_conting_bin_neg0.05,
                                                                obs_values = obs_metric[["score_neg"]],
                                                                Nrun       = 1,
                                                                save       = TRUE,
                                                                name       = "sensitivity_analysis_res_neg0.05")


sensitivity_analysis_neg0.1 <- NCSSDGproj::sensitivity_analysis(matrix_rep = matrix_conting_bin_neg0.1,
                                                                obs_values = obs_metric[["score_neg"]],
                                                                Nrun       = 1,
                                                                save       = TRUE,
                                                                name       = "sensitivity_analysis_res_neg0.1")


    
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
  SDG_info_neg <- NCSSDGproj::SDG_infos(matrix_cont = matrix_conting_bin[["score_neg"]]) 
  
  ## ---- SDG icons
  icon_SDG <- NCSSDGproj::format_icons(pathSDG, icon_SDG = TRUE)
  
  
  
  
### ----- ANALYSIS
  
  ## ---- Compute target's insurance
    
    # --- POSITIVE results
    data_Insurance <- NCSSDGproj::Insurance_data2plot(matrix01 = matrix_conting_bin[["score_pos"]], 
                                                      Ntarget  = ncol(matrix_conting_bin[["score_pos"]])) 
    
    # --- NEGATIVE results
    data_Insurance_neg <- NCSSDGproj::Insurance_data2plot(matrix01 = matrix_conting_bin[["score_neg"]], 
                                                          Ntarget  = ncol(matrix_conting_bin[["score_neg"]])) 
  
  ## ---- Format data to plot
    
    # --- POSITIVE results
    data_circu <- NCSSDGproj::circular_data_Insurance(data_Insurance = data_Insurance, 
                                                      data_long = data_long[["score_pos"]], 
                                                      SDG_info = SDG_info, 
                                                      NCS_info = NCS_info) # format data with polar coordinates

    # --- NEGATIVE results
    data_circu_neg <- NCSSDGproj::circular_data_Insurance(data_Insurance = data_Insurance_neg, 
                                                          data_long = data_long[["score_neg"]], 
                                                          SDG_info = SDG_info_neg, 
                                                          NCS_info = NCS_info) # format data with polar coordinates
        
        
### ----- PLOT DATA
    
  ## ---- POSITIVE data
  NCSSDGproj::circular_plot_Insurance(data         = data_circu[[1]], 
                                      label_data   = data_circu[[2]],
                                      base_data    = data_circu[[3]],
                                      grid_data    = data_circu[[4]],
                                      SDG_info     = SDG_info,
                                      colNCS_ter   = "#228B22", 
                                      colNCS_coast = "#5EA9A2",
                                      colNCS_mar   = "#1134A6",
                                      icon_SDG      = icon_SDG,
                                      save         = TRUE,
                                      name         = "Figure4_pos_V3")   

  ## ---- NEGATIVE data (Supplementary)
  NCSSDGproj::circular_plot_Insurance_neg(data         = data_circu_neg[[1]], 
                                          label_data   = data_circu_neg[[2]],
                                          base_data    = data_circu_neg[[3]],
                                          grid_data    = data_circu_neg[[4]],
                                          SDG_info     = SDG_info_neg,
                                          colNCS_ter   = "#228B22", 
                                          colNCS_coast = "#5EA9A2",
                                          colNCS_mar   = "#1134A6",
                                          icon_SDG      = icon_SDG,
                                          save         = TRUE,
                                          name         = "Figure4_neg_V4")   

    
    
    
    
    
     

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

#########################
# Supplementary Figures #
#########################

### Supplementary Fig. 2 - % of ones by ecosytems
NCSSDGproj::percentage_of_ones(data_pos = matrix_all[["score_pos"]], 
                               data_neg = matrix_all[["score_neg"]],
                               save     = TRUE,
                               name     = "supp_fig_perc_of_ones")

### Supplementary Fig. 3 - number of links by ecosystem AND/OR Supplementary Fig. 4 - biplot n positive vs. n negative links
rm(list = ls(), envir = .GlobalEnv)    

  ## Data
  sheets  <- NCSSDGproj::read_all_sheets()
  matrix_all <- NCSSDGproj::sheets_to_matrix(sheets_list = sheets, binary = TRUE)
  
  ## Plot 
  NCSSDGproj::supp_plot_n_links(data_pos = matrix_all[["score_pos"]], 
                                data_neg = matrix_all[['score_neg']],
                                save     = TRUE,
                                name1    = "n_links_barplot",
                                biplot   = TRUE,
                                name2    = "n_links_biplot")


  
##########################
# Supplementary Analyses #
##########################
  
### Modularity and nestedness at the SDG level
  
  ## Load data
  sheets  <- NCSSDGproj::read_all_sheets()

  ## Format data
  matrix_all <- NCSSDGproj::sheets_to_matrix(sheets_list = sheets, binary = TRUE)
  
    # Positive links
    data_long_pos <- NCSSDGproj::matrix_to_longDF(matrix01 = matrix_all[["score_pos"]])
    SDG_matrix_pos  <- NCSSDGproj::matrix_SDG(data_long = data_long_pos)
    raw_data_pos <- cbind(rownames(SDG_matrix_pos), as.data.frame(SDG_matrix_pos))
    colnames(raw_data_pos)[1] <- "ecosystem"
  
    # Negative links
    data_long_neg <- NCSSDGproj::matrix_to_longDF(matrix01 = matrix_all[["score_neg"]])
    SDG_matrix_neg  <- NCSSDGproj::matrix_SDG(data_long = data_long_neg)
    raw_data_neg <- cbind(rownames(SDG_matrix_neg), as.data.frame(SDG_matrix_neg))
    colnames(raw_data_neg)[1] <- "ecosystem"  
  
  ## Analyses
    
    # Modularity and Nestedness positive data
    NCSSDGproj::NullModels(matrix01        = SDG_matrix_pos, 
                           rawdata         = raw_data_pos, 
                           NMalgo          = "quasiswap_count", # r2dtable different results
                           NESTmethod      = "NODF",
                           Nrun            = 1, # Nrun = 10 in the paper
                           Nsim            = 999, # Nsim = 999 in the paper - TAKES TIME TO RUN
                           TargetInsurance = FALSE,
                           save            = TRUE,
                           name            = "Nest_Modu_SDGlevel_pos")
    
    # Modularity and Nestedness negative data
    NCSSDGproj::NullModels(matrix01        = SDG_matrix_neg, 
                           rawdata         = raw_data_neg, 
                           NMalgo          = "quasiswap_count", # quasiswap_count different results
                           NESTmethod      = "NODF",
                           Nrun            = 1, # Nrun = 10 in the paper
                           Nsim            = 999, # Nsim = 999 in the paper - TAKES TIME TO RUN
                           TargetInsurance = FALSE,
                           save            = TRUE,
                           name            = "Nest_Modu_SDGlevel_neg")
    
    
    # TUI and TOI positive data
    SDG_matrix_pos[SDG_matrix_pos > 0] <- 1
    raw_data_pos <- cbind(rownames(SDG_matrix_pos), as.data.frame(SDG_matrix_pos))
    colnames(raw_data_pos)[1] <- "ecosystem"
    NCSSDGproj::NullModels(matrix01        = SDG_matrix_pos, 
                           rawdata         = raw_data_pos, 
                           NMalgo          = "r00",
                           Nsim            = 999, # Nsim = 999 in the paper - TAKES TIME TO RUN
                           TargetInsurance = TRUE,
                           save            = TRUE,
                           name            = "TUI_TOI_SDGlevel_pos")
    
    # TUI and TOI negative data
    SDG_matrix_neg[SDG_matrix_neg > 0] <- 1
    raw_data_neg <- cbind(rownames(SDG_matrix_neg), as.data.frame(SDG_matrix_neg))
    colnames(raw_data_neg)[1] <- "ecosystem"    
    NCSSDGproj::NullModels(matrix01        = SDG_matrix_neg, 
                           rawdata         = raw_data_neg, 
                           NMalgo          = "r00",
                           Nsim            = 999, # Nsim = 999 in the paper - TAKES TIME TO RUN
                           TargetInsurance = TRUE,
                           save            = TRUE,
                           name            = "TUI_TOI_SDGlevel_neg")
            
  