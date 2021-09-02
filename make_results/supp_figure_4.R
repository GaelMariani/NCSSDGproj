#######################################
#                                     #
#         produce supp. Fig 4         #
#                                     #
####################################### 
rm(list = ls(), envir = .GlobalEnv)  


### ----- LOAD DATA
null_vals <- NCSSDGproj::load_metric_obs(null_vals = TRUE)[["score_neg"]][, -4]
res_null_mod <- NCSSDGproj::load_metric_obs(null_vals = FALSE)[["score_neg"]]


### ----- PLOT DATA
NCSSDGproj::supp_fig_null_hist(null_vals    = null_vals, 
                               res_null_mod = res_null_mod,
                               name         = "Supp_fig4")
