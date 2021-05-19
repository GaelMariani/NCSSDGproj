#######################################
#                                     #
# produce supp. Fig 3 and supp. Fig 4 #
#                                     #
####################################### 
rm(list = ls(), envir = .GlobalEnv)  


### LOAD Data
sheets  <- NCSSDGproj::read_all_sheets()
matrix_all <- NCSSDGproj::sheets_to_df(sheets_list = sheets, binary = TRUE)

### PLOT
NCSSDGproj::supp_plot_n_links(data_pos = matrix_all[["score_pos"]], 
                              data_neg = matrix_all[['score_neg']],
                              save     = TRUE,
                              name1    = "Supp_fig3",
                              biplot   = TRUE,
                              name2    = "Supp_fig4")