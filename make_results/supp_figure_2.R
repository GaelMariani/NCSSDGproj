####################################################################
#                                                                  #
# produce supp. Fig 1 - Correspondance Analysis for positive links #
#                                                                  #
####################################################################
rm(list = ls(), envir = .GlobalEnv)


### ----- LOAD DATA

  ## ---- Data of links between NCS and SDG
  sheets  <- NCSSDGproj::read_all_sheets()

### ----- FORMAT DATA

  ## ---- From sheets to df
  matrix_all <- NCSSDGproj::sheets_to_df(sheets_list = sheets, binary = TRUE)
  
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

  ## ---- Correspondance analysis for POSITIVE links with the contribution of each target and NCS to the variance of each axis
  contri_CA_neg <- NCSSDGproj::CA_contri_vars(matrix_cont  = matrix_conting_bin[["score_neg"]],
                                              axis2_targ   = axis2_targ,
                                              colNCS_ter   = "#228B22",
                                              colNCS_coast = "#5EA9A2",
                                              colNCS_mar   = "#1134A6")

### PLOT DATA 

  ## ---- Figure with all panels for POSITIVE links
  NCSSDGproj::supp_fig1(data           = contri_CA_neg[["CorresAna"]],
                        targ_contrib12 = contri_CA_neg[["col_contrib"]][["axe12"]],
                        arrow          = FALSE,
                        data_arrow     = contri_CA_neg[["data_arrow"]],
                        colNCS_ter     = "#228B22", 
                        colNCS_coast   = "#5EA9A2",
                        colNCS_mar     = "#1134A6",
                        save           = TRUE,
                        name           = "Supp_fig2")
  