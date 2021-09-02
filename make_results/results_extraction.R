#########################################
#                                       #
# Extract all numbers used in the paper #
#                                       #
#########################################
rm(list = ls(), envir = .GlobalEnv)    


### ----- LOAD DATA (rawdata)
sheets  <- NCSSDGproj::read_all_sheets()


### ----- FORMAT DATA
df_all <- NCSSDGproj::sheets_to_df(sheets_list = sheets, binary = TRUE)

data_long_pos   <- NCSSDGproj::df_to_longDF(df = df_all[["score_pos"]])
SDG_matrix_pos  <- t(NCSSDGproj::matrix_SDG(data_long = data_long_pos))
data_pourc_pos  <- NCSSDGproj::perc_SDG(data_long = data_long_pos)

data_long_neg   <- NCSSDGproj::df_to_longDF(df = df_all[["score_neg"]])
SDG_matrix_neg  <- NCSSDGproj::matrix_SDG(data_long = data_long_neg)
data_pourc_neg  <- NCSSDGproj::perc_SDG(data_long = data_long_neg)


### ----- Results in section - Relationships between NCS implementation and SDG target achievement -

  ## ---- Number of positive and negative links
  sum(df_all[["score_pos"]][, -1])
  sum(df_all[["score_neg"]][, -1])
  
  ## ---- Number of SDG linked to NCS implementation
  sum(rowSums(SDG_matrix_pos) >= 1)
  sum(rowSums(SDG_matrix_neg) >= 1)
  
  ## ---- % of SDG linked to NCS implementation
  sum(rowSums(SDG_matrix_pos) != 1)/nrow(SDG_matrix_pos)*100
  sum(colSums(SDG_matrix_neg) != 0)/ncol(SDG_matrix_neg)*100
  
  ## ---- Number of SDG targets linked to NCS implementation
  sum(colSums(df_all[["score_cumulate"]][, -1]) != 0)
  sum(colSums(df_all[["score_pos"]][, -1]) != 0)
  sum(colSums(df_all[["score_neg"]][, -1]) != 0)
  
  ## ---- % of SDG targets linked to NCS implementation
  sum(colSums(df_all[["score_cumulate"]][, -1]) != 0)/ncol(df_all[["score_cumulate"]][, -1])*100
  sum(colSums(df_all[["score_pos"]][, -1]) != 0)/ncol(df_all[["score_pos"]][, -1])*100
  sum(colSums(df_all[["score_neg"]][, -1]) != 0)/ncol(df_all[["score_neg"]][, -1])*100
  
  ## ---- Mean number of SDG targets inside each SDG impacted by NCS implementation
  mean(data_pourc_pos$perc_goal[seq(1, nrow(data_pourc_pos), 3)])
  sd(data_pourc_pos$perc_goal[seq(1, nrow(data_pourc_pos), 3)])
  
  mean(data_pourc_neg$perc_goal[seq(1, nrow(data_pourc_neg), 3)])
  sd(data_pourc_neg$perc_goal[seq(1, nrow(data_pourc_neg), 3)])


### ----- Results in sections - Complementarity of NCS in SDG targets achievement - & - SDG target insurance - 

  ## ---- Modularity and Nestedness 
  
    # --- Target level
    load(here::here("results", "Nest_Modu_res_neg.RData"))
    NM_neg <- res
    
    load(here::here("results", "Nest_Modu_res_pos.RData"))
    NM_pos <- res
    
    # --- SDG level
    load(here::here("results", "Nest_Modu_SDGlevel_neg.RData"))
    NM_neg_SDG <- res
    
    load(here::here("results", "Nest_Modu_SDGlevel_pos.RData"))
    NM_pos_SDG <- res

  ## ---- Insurance
  
    # --- Target level
    load(here::here("results", "TUI_TOI_res_neg.RData"))
    TUI_neg <- res
    
    load(here::here("results", "TUI_TOI_res_pos.RData"))
    TUI_pos <- res
    
    # --- SDG level
    load(here::here("results", "TUI_TOI_SDGlevel_neg.RData"))
    TUI_neg_SDG <- res
    
    load(here::here("results", "TUI_TOI_SDGlevel_pos.RData"))
    TUI_pos_SDG <- res
  
  ## ---- Sensitivity analysis
  
    # --- Negative links
    load(here::here("results", "sensitivity_analysis_res_neg0.1.RData"))
    SA_neg <- sensit_ana_res
    
    
    # --- Positive links
    load(here::here("results", "sensitivity_analysis_res_pos0.1.RData"))
    SA_pos <- sensit_ana_res
    

### ----- Results in section - Research gaps and insights - 

  ## ---- Positive links
  data.frame(Ecosystem = df_all[["score_pos"]][,1], n_links = rowSums(df_all[["score_pos"]][,-1]))
  
  ## ---- Negative links
  data.frame(Ecosystem = df_all[["score_neg"]][,1], n_links = rowSums(df_all[["score_neg"]][,-1]))
  


