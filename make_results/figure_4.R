############################################################
#                                                          #
# produce FIGURE 4 - Target's insurance for negative links #
#                                                          #
############################################################ 

rm(list = ls(), envir = .GlobalEnv)    

### ----- LOAD DATA

  ## ---- Data of links between NCS and SDG
  sheets  <- NCSSDGproj::read_all_sheets()
  
  ## ---- SDG icons
  pathSDG <- NCSSDGproj::load_SDG_icon()
  
  ### ----- FORMAT DATA
  
  ## ---- From sheets to df
  matrix_all <- NCSSDGproj::sheets_to_df(sheets_list = sheets, binary = TRUE)
  
  ## ---- From dataframes to contingency matrices 
  matrix_conting_bin <- lapply(matrix_all, NCSSDGproj::contingency_mat_targets, binary = TRUE)
  
  ## ---- Format data into a long data frame
  data_long <- lapply(matrix_all, NCSSDGproj::df_to_longDF)
  
  ## ---- Informations on NCSs
  NCS_info <- NCSSDGproj::NCS_info(matrix_cont = matrix_conting_bin[["score_pos"]])
  
  ## ---- Informations on SDGs
  SDG_info_neg <- NCSSDGproj::SDG_infos(matrix_cont = matrix_conting_bin[["score_neg"]]) 
  
  ## ---- SDG icons
  icon_SDG <- NCSSDGproj::format_icons(pathSDG, icon_SDG = TRUE)
  
  
### ----- ANALYSIS
  
  ## ---- Compute target's insurance
  data_Insurance_neg <- NCSSDGproj::Insurance_data2plot(matrix01 = matrix_conting_bin[["score_neg"]], 
                                                        Ntarget  = ncol(matrix_conting_bin[["score_neg"]])) 
  
  ## ---- Format data to plot
  data_circu_neg <- NCSSDGproj::circular_data_Insurance(data_Insurance = data_Insurance_neg, 
                                                        data_long = data_long[["score_neg"]], 
                                                        SDG_info = SDG_info_neg, 
                                                        NCS_info = NCS_info) # format data with polar coordinates
  
  
### ----- PLOT DATA
NCSSDGproj::circular_plot_Insurance_neg(data         = data_circu_neg[[1]], 
                                        label_data   = data_circu_neg[[2]],
                                        base_data    = data_circu_neg[[3]],
                                        grid_data    = data_circu_neg[[4]],
                                        SDG_info     = SDG_info_neg,
                                        colNCS_ter   = "#228B22", 
                                        colNCS_coast = "#5EA9A2",
                                        colNCS_mar   = "#1134A6",
                                        icon_SDG     = icon_SDG,
                                        save         = TRUE,
                                        name         = "Figure4_neg_V5")  
  
  
  