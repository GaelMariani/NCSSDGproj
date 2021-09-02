#########################################################################
#                                                                       #
#         produce supp. Fig 9 and 10 - sensitivity analyses 20p         #
#                                                                       #
######################################################################### 
rm(list = ls(), envir = .GlobalEnv)  


### Load Data
pos <- get(load(here::here("results", "sensitivity_analysis_res_pos0.2.RData")))
neg <- get(load(here::here("results", "sensitivity_analysis_res_neg0.2.RData")))


### Format Data

## Positive values
sensit_ana_res_pos <- pos[["res_analyses"]]
null_values_pos <- pos[["null_values"]]

## Negative values
sensit_ana_res_neg <- neg[["res_analyses"]]
null_values_neg <- neg[["null_values"]]

### Plot

## Positive values
NCSSDGproj::supp_fig_null_hist(null_vals    = null_values_pos, 
                               res_null_mod = sensit_ana_res_pos,
                               name         = "Supp_fig9_sensit_pos_20p_unused")


## Negative values
NCSSDGproj::supp_fig_null_hist(null_vals    = null_values_neg, 
                               res_null_mod = sensit_ana_res_neg,
                               name         = "Supp_fig10_sensit_neg_20p_unused")
