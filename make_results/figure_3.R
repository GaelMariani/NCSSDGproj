###################################################################################
#                                                                                 #
# produce FIGURE 3 - Relationships between NCS implementation and SDG achievement #
#                                                                                 #
###################################################################################
rm(list = ls(), envir = .GlobalEnv)


### ----- LOAD DATA (rawdata and icons)
sheets  <- NCSSDGproj::read_all_sheets()
pathSDG <- NCSSDGproj::load_SDG_icon()
pathNCS <- NCSSDGproj::load_NCS_icon()


### ----- FORMAT ICONS (NCS and SDG icons)
icon_SDG <- NCSSDGproj::format_icons(pathSDG, icon_SDG = TRUE)
icon_NCS <- NCSSDGproj::format_icons(pathNCS, icon_SDG = FALSE)


### ----- FORMAT DATA
df_all <- NCSSDGproj::sheets_to_df(sheets_list = sheets, binary = TRUE)

  ## ---- Positive links
  data_long_pos   <- NCSSDGproj::df_to_longDF(df = df_all[["score_pos"]])
  SDG_matrix_pos  <- t(NCSSDGproj::matrix_SDG(data_long = data_long_pos))
  SDG_network_pos <- NCSSDGproj::matrix_to_network(matrix = SDG_matrix_pos,
                                                   mode1  = "P",
                                                   mode2  = "A",
                                                   neg    = FALSE)
  data_pourc_pos  <- NCSSDGproj::perc_SDG(data_long = data_long_pos)
  
  
  ## ---- Negative links
  data_long_neg   <- NCSSDGproj::df_to_longDF(df = df_all[["score_neg"]])
  SDG_matrix_neg  <- NCSSDGproj::matrix_SDG(data_long = data_long_neg)
  SDG_network_neg <- NCSSDGproj::matrix_to_network(matrix = SDG_matrix_neg,
                                                   mode1  = "P",
                                                   mode2  = "A",
                                                   neg    = TRUE)
  data_pourc_neg  <- NCSSDGproj::perc_SDG(data_long = data_long_neg)
  
  ## ---- merge data
  SDG_network <- list("score_pos" = list(data_long = data_long_pos, matrix = SDG_matrix_pos, network = SDG_network_pos, data_pourc = data_pourc_pos),
                      "score_neg" = list(data_long = data_long_neg, matrix = SDG_matrix_neg, network = SDG_network_neg, data_pourc = data_pourc_neg))
  

### ----- PRODUCE FIGURE 2
NCSSDGproj::Figure2(save = TRUE,
                    name = "Figure3_FFE_final")


NCSSDGproj::barplot_perc_achieve(SDG_network = SDG_network,
                                 color = c("#1134A6", "#5EA9A2",  "#228B22", "#1134A6", "#5EA9A2",  "#228B22"),
                                 # color       = rep(c("darkgreen", "seagreen4",  "springgreen3", "palegreen",
                                 #                     "navajowhite4", "navajowhite3", "navajowhite2", "navajowhite",
                                 #                     "royalblue4", "royalblue3", "steelblue3", "skyblue"), 2), # Mar, Coast, Ter, Mar_neg, Coast_neg, Ter_neg
                                 save        = FALSE,
                                 name        = "Figure3_FEE_test")

