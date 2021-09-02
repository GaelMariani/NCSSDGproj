#######################################################################################
#                                                                                     #
# SENSITIVITY ANALYSIS for Modularity + Nestedness + Insurance - 20% of modified data #
#                                                                                     #
#######################################################################################    
rm(list = ls(), envir = .GlobalEnv)


### ----- LOAD DATA

  ## ---- Data of links between NCS and SDG
  sheets  <- NCSSDGproj::read_all_sheets()
  
  ## Observed modularity and nestedness values
  obs_metric <- NCSSDGproj::load_metric_obs(null_vals = FALSE)


### ----- FORMAT DATA
set.seed(2511) # for reproductibility

  ## ---- From sheets to df
  matrix_all <- NCSSDGproj::sheets_to_df(sheets_list = sheets, binary = FALSE) 
  
  matrices_modif_0.2 <- list(
    # Positive scores
    "score_pos" = replicate(n        = 999, # n = 999 in the paper
                            simplify = FALSE,
                            expr     =  NCSSDGproj::turn_values_randomly(data_links = matrix_all[["score_pos"]],
                                                                         percentage = 0.1, 
                                                                         binary     = TRUE)),
    
    # Negative scores
    "score_neg" = replicate(n        = 999, # n = 999 in the paper
                            simplify = FALSE,
                            expr     = NCSSDGproj::turn_values_randomly(data_links = matrix_all[["score_neg"]],
                                                                        percentage = 0.1, 
                                                                        binary     = TRUE)))
  
    ## ---- From dataframes to contingency matrices
    matrix_conting_bin_pos0.2 <- lapply(matrices_modif_0.2[["score_pos"]], NCSSDGproj::contingency_mat_targets, binary = TRUE)
    matrix_conting_bin_neg0.2 <- lapply(matrices_modif_0.2[["score_neg"]], NCSSDGproj::contingency_mat_targets, binary = TRUE)
  

### ----- ANALYSES
sensitivity_analysis_pos0.2 <- NCSSDGproj::sensitivity_analysis(matrix_rep = matrix_conting_bin_pos0.2,
                                                                obs_values = obs_metric[["score_pos"]],
                                                                Nrun       = 5, # Nrun = 5 in the paper
                                                                save       = TRUE,
                                                                name       = "sensitivity_analysis_res_pos0.2")
print(sensitivity_analysis_pos0.2)



sensitivity_analysis_neg0.2 <- NCSSDGproj::sensitivity_analysis(matrix_rep = matrix_conting_bin_neg0.2,
                                                                obs_values = obs_metric[["score_neg"]],
                                                                Nrun       = 5, # Nrun = 5 in the paper
                                                                save       = TRUE,
                                                                name       = "sensitivity_analysis_res_neg0.2")
print(sensitivity_analysis_neg0.2)

