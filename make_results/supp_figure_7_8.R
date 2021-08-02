########################################################################
#                                                                      #
#         produce supp. Fig 7 and 8 - sensitivity analyses 10p         #
#                                                                      #
######################################################################## 
rm(list = ls(), envir = .GlobalEnv)  


### Load Data
pos <- get(load(here::here("results", "sensitivity_analysis_res_pos0.1.RData")))
neg <- get(load(here::here("results", "sensitivity_analysis_res_neg0.1.RData")))


### Format Data

  ## Positive values
  sensit_ana_res_pos <- pos[["res_analyses"]]
  null_values_pos <- pos[["null_values"]]
  
  ## Negative values
  sensit_ana_res_neg <- neg[["res_analyses"]]
  null_values_neg <- neg[["null_values"]]

### Plot
  
  ## Positive values
  NCSSDGproj::supp_fig5to8(null_vals    = null_values_pos, 
                           res_null_mod = sensit_ana_res_pos,
                           name         = "Supp_fig7_sensit_pos_10p")
  
  
  ## Negative values
  NCSSDGproj::supp_fig5to8(null_vals    = null_values_neg, 
                           res_null_mod = sensit_ana_res_neg,
                           name         = "Supp_fig8_sensit_neg_10p")
