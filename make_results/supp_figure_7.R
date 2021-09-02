#######################
#                     #
# produce supp. Fig 7 #
#                     #
#######################
rm(list = ls(), envir = .GlobalEnv)  


### LOAD Data
sheets  <- NCSSDGproj::read_all_sheets()
matrix_all <- NCSSDGproj::sheets_to_df(sheets_list = sheets, binary = TRUE)

### PLOT
NCSSDGproj::supp_fig7(data_pos = matrix_all[["score_pos"]], 
                      data_neg = matrix_all[['score_neg']],
                      save     = TRUE,
                      name1    = "Supp_fig7",
                      biplot   = TRUE,
                      name2    = "Supp_fig8_unsused")
