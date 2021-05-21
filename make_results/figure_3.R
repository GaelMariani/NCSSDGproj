############################################################
#                                                          #
# produce FIGURE 3 - Target's insurance for positive links #
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
  matrix_all <- NCSSDGproj::sheets_to_df(sheets_list = sheets, 
                                         binary      = TRUE)
  
  ## ---- From dataframes to contingency matrices 
  matrix_conting_bin <- lapply(matrix_all, NCSSDGproj::contingency_mat_targets, binary = TRUE)
  
  ## ---- Format data into a long data frame
  data_long <- lapply(matrix_all, NCSSDGproj::df_to_longDF)
  
  ## ---- Informations on NCSs
  NCS_info <- NCSSDGproj::NCS_info(matrix_cont = matrix_conting_bin[["score_pos"]])
  
  ## ---- Informations on SDGs
  SDG_info <- NCSSDGproj::SDG_infos(matrix_cont = matrix_conting_bin[["score_pos"]])

  ## ---- SDG icons
  icon_SDG <- NCSSDGproj::format_icons(pathSDG, icon_SDG = TRUE)


### ----- ANALYSES and FORMATING

  ## ---- Compute target's insurance
  data_Insurance <- NCSSDGproj::Insurance_data2plot(matrix01 = matrix_conting_bin[["score_pos"]], 
                                                    Ntarget  = ncol(matrix_conting_bin[["score_pos"]])) 
    
  ## ---- Format data to plot
  data_circu <- NCSSDGproj::circular_data_Insurance(data_Insurance = data_Insurance, 
                                                    data_long      = data_long[["score_pos"]], 
                                                    SDG_info       = SDG_info, 
                                                    NCS_info       = NCS_info) # format data with polar coordinates
  
### ----- PLOT DATA
NCSSDGproj::circular_plot_Insurance(data         = data_circu[[1]], 
                                    label_data   = data_circu[[2]],
                                    base_data    = data_circu[[3]],
                                    grid_data    = data_circu[[4]],
                                    SDG_info     = SDG_info,
                                    colNCS_ter   = "#228B22", 
                                    colNCS_coast = "#5EA9A2",
                                    colNCS_mar   = "#1134A6",
                                    icon_SDG     = icon_SDG,
                                    save         = TRUE,
                                    name         = "Figure3")   

  
  
  