########################################################################
#                                                                      #
# NULL MODELS for Modularity + Nestedness + Insurance at the SDG level #
#                                                                      #
######################################################################## 
rm(list = ls(), envir = .GlobalEnv)


### ----- LOAD DATA
sheets  <- NCSSDGproj::read_all_sheets()

### ----- FORMAT DATA
matrix_all <- NCSSDGproj::sheets_to_df(sheets_list = sheets, binary = TRUE)

  ## ---- Positive links
  data_long_pos <- NCSSDGproj::df_to_longDF(df = matrix_all[["score_pos"]])
  SDG_matrix_pos  <- NCSSDGproj::matrix_SDG(data_long = data_long_pos)
  raw_data_pos <- cbind(rownames(SDG_matrix_pos), as.data.frame(SDG_matrix_pos))
  colnames(raw_data_pos)[1] <- "ecosystem"
  
  ## ---- Negative links
  data_long_neg <- NCSSDGproj::df_to_longDF(df = matrix_all[["score_neg"]])
  SDG_matrix_neg  <- NCSSDGproj::matrix_SDG(data_long = data_long_neg)
  raw_data_neg <- cbind(rownames(SDG_matrix_neg), as.data.frame(SDG_matrix_neg))
  colnames(raw_data_neg)[1] <- "ecosystem"  

### ----- ANALYSES
set.seed(2511)

  ## ---- Modularity and Nestedness positive data
  Nest_Modu_SDGlevel_pos <- NCSSDGproj::NullModels(matrix01        = SDG_matrix_pos, 
                                                   rawdata         = raw_data_pos, 
                                                   NMalgo          = "quasiswap_count", # r2dtable different results
                                                   NESTmethod      = "NODF",
                                                   Nrun            = 5, # Nrun = 5 in the paper
                                                   Nsim            = 999, # Nsim = 999 in the paper - TAKES TIME TO RUN
                                                   TargetInsurance = FALSE,
                                                   save            = TRUE,
                                                   name            = "Nest_Modu_SDGlevel_pos")
  
  print(Nest_Modu_SDGlevel_pos)
  
  ## ---- Modularity and Nestedness negative data
  Nest_Modu_SDGlevel_neg <- NCSSDGproj::NullModels(matrix01        = SDG_matrix_neg, 
                                                   rawdata         = raw_data_neg, 
                                                   NMalgo          = "quasiswap_count", # quasiswap_count different results
                                                   NESTmethod      = "NODF",
                                                   Nrun            = 5, # Nrun = 5 in the paper
                                                   Nsim            = 999, # Nsim = 999 in the paper - TAKES TIME TO RUN
                                                   TargetInsurance = FALSE,
                                                   save            = TRUE,
                                                   name            = "Nest_Modu_SDGlevel_neg")
  
  print(Nest_Modu_SDGlevel_neg)
  
  
  ## ---- TUI and TOI positive data
  SDG_matrix_pos[SDG_matrix_pos > 0] <- 1
  raw_data_pos <- cbind(rownames(SDG_matrix_pos), as.data.frame(SDG_matrix_pos))
  colnames(raw_data_pos)[1] <- "ecosystem"
  
  TUI_TOI_SDGlevel_pos <- NCSSDGproj::NullModels(matrix01        = SDG_matrix_pos, 
                                                 rawdata         = raw_data_pos, 
                                                 NMalgo          = "r00",
                                                 Nsim            = 999, # Nsim = 999 in the paper - TAKES TIME TO RUN
                                                 TargetInsurance = TRUE,
                                                 save            = TRUE,
                                                 name            = "TUI_TOI_SDGlevel_pos")
  
  print(TUI_TOI_SDGlevel_pos)
  
  
  ## ---- TUI and TOI negative data
  SDG_matrix_neg[SDG_matrix_neg > 0] <- 1
  raw_data_neg <- cbind(rownames(SDG_matrix_neg), as.data.frame(SDG_matrix_neg))
  colnames(raw_data_neg)[1] <- "ecosystem"  
  
  TUI_TOI_SDGlevel_neg <- NCSSDGproj::NullModels(matrix01        = SDG_matrix_neg, 
                                                 rawdata         = raw_data_neg, 
                                                 NMalgo          = "r00",
                                                 Nsim            = 999, # Nsim = 999 in the paper - TAKES TIME TO RUN
                                                 TargetInsurance = TRUE,
                                                 save            = TRUE,
                                                 name            = "TUI_TOI_SDGlevel_neg")
  
  print(TUI_TOI_SDGlevel_neg)
  
  
                          