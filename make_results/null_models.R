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
  matrix_all <- NCSSDGproj::sheets_to_df(sheets_list = sheets, binary = TRUE)
  
  ## ---- From dataframes to contingency matrices 
  matrix_conting_bin <- lapply(matrix_all, NCSSDGproj::contingency_mat_targets, binary = TRUE)
  
  
  
### ----- ANALYSIS
  
  ## ---- Modularity and Nestedness - NMalgo = "quasiswap" to conserve marginal sums
  
    # --- POSITIVE scores
    Nest_mod_res_pos <- NCSSDGproj::NullModels(matrix01        = matrix_conting_bin[["score_pos"]], 
                                               rawdata         = matrix_all[["score_pos"]], 
                                               NMalgo          = "quasiswap", 
                                               NESTmethod      = "NODF",
                                               Nrun            = 1, # Nrun = 5 in the paper
                                               Nsim            = 99, # Nsim = 999 in the paper - TAKES TIME TO RUN
                                               TargetInsurance = FALSE,
                                               save            = TRUE,
                                               name            = "Nest_Modu_res_pos")
    
    print(Nest_mod_res_pos)
    
    # --- NEGATIVE scores
    Nest_mod_res_neg <- NCSSDGproj::NullModels(matrix01        = matrix_conting_bin[["score_neg"]], 
                                               rawdata         = matrix_all[["score_neg"]], 
                                               NMalgo          = "quasiswap", 
                                               NESTmethod      = "NODF",
                                               Nrun            = 1, # Nrun = 5 in the paper
                                               Nsim            = 99, # Nsim = 999 in the paper - TAKES TIME TO RUN
                                               TargetInsurance = FALSE,
                                               save            = TRUE,
                                               name            = "Nest_Modu_res_neg")
    print(Nest_mod_res_neg)
  
  ## ---- Insurance - NMalgo = "r00" to not conserve marginal sums
  
    # --- POSITIVE scores
    TUI_TOI_res_pos <- NCSSDGproj::NullModels(matrix01        = matrix_conting_bin[["score_pos"]], 
                                              rawdata         = matrix_all[["score_pos"]], 
                                              NMalgo          = "r00",
                                              Nsim            = 999, # Nsim = 999 in the paper - TAKES TIME TO RUN
                                              TargetInsurance = TRUE,
                                              save            = TRUE,
                                              name            = "TUI_TOI_res_pos")
    print(TUI_TOI_res_pos)
  
  
    # --- NEGATIVE scores
    TUI_TOI_res_neg <- NCSSDGproj::NullModels(matrix01        = matrix_conting_bin[["score_neg"]], 
                                              rawdata         = matrix_all[["score_neg"]], 
                                              NMalgo          = "r00", 
                                              Nsim            = 999, # Nsim = 999 in the paper - TAKES TIME TO RUN
                                              TargetInsurance = TRUE,
                                              save            = TRUE,
                                              name            = "TUI_TOI_res_neg")
    print(TUI_TOI_res_neg)
  