#######################################
#                                     #
#         produce supp. Fig 5         #
#                                     #
####################################### 
rm(list = ls(), envir = .GlobalEnv)  


### Load Data
null_vals <- NCSSDGproj::load_metric_obs(null_vals = TRUE)[["score_pos"]][, -4]
res_null_mod <- NCSSDGproj::load_metric_obs(null_vals = FALSE)[["score_pos"]]


### Plot
NCSSDGproj::supp_fig5to8(null_vals    = null_vals, 
                         res_null_mod = res_null_mod,
                         name         = "Supp_fig5")
