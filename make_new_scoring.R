####################################################################################
#                                                                                  #
# Data and Code for NCS - SDG analysis with the new scoring system (from -2 to +2) #
#                                                                                  #
####################################################################################

### ----- clean workspace
rm(list = ls(), envir = .GlobalEnv)

### ----- install devtools
install.packages("devtools")

### ----- install/update packages (1 if you want to install all packages)
devtools::install_deps()

### ----- load functions in the compendium
devtools::load_all()

## ---- in case of any problem with the installation of the package GGally try
devtools::install_github("ggobi/ggally")


###################################################################################
#                                                                                 #
# produce FIGURE 2 - Relationships between NCS implementation and SDG achievement #
#                                                                                 #
###################################################################################

### ----- Load rawdata and icons
sheets  <- NCSSDGproj::read_all_sheets()
pathSDG <- NCSSDGproj::load_SDG_icon()
pathNCS <- NCSSDGproj::load_NCS_icon()


### ----- Format icons (NCS and SDG icons)
icon_SDG <- NCSSDGproj::format_icons(pathSDG, icon_SDG = TRUE)
icon_NCS <- NCSSDGproj::format_icons(pathNCS, icon_SDG = FALSE)
  

### ----- Format data
matrix_all <- NCSSDGproj::sheets_to_matrix(sheets_list = sheets)

SDG_network <- lapply(1:length(matrix_all), 
                      function(i){
                        
                        # -- matrix in long format
                        data_long   <- NCSSDGproj::matrix_to_longDF(matrix01 = matrix_all[[i]])
                        
                        # -- weighted contingency matrix of SDG
                        SDG_matrix  <- NCSSDGproj::matrix_SDG(data_long = data_long)
                        
                        # -- create a network object
                        SDG_network <- NCSSDGproj::matrix_to_network(matrix = SDG_matrix,
                                                                     mode1  = "P",
                                                                     mode2  = "A")
                        
                        # -- percentage of target achieved
                        data_pourc  <- NCSSDGproj::perc_SDG(data_long = data_long)
                        
                        return(list(data_long = data_long, matrix = SDG_matrix, network = SDG_network, data_pourc = data_pourc))
                        
                      })
names(SDG_network) <- names(matrix_all)


### ----- Plot data

  ## ---- Plot panel A - the bipartite network
  NCSSDGproj::plot_network(network_obj = SDG_network[["score_cumulate"]][["network"]],
                           matrix      = SDG_network[["score_cumulate"]][["matrix"]],
                           icon_SDG    = icon_SDG,
                           icon_NCS    = icon_NCS,
                           nodes_col   = c(rep("#228B22", 4), rep("#5EA9A2", 4), rep("#1134A6", 3)),
                           save        = TRUE)
    
  ## ---- Plot panel B - the barplot
  NCSSDGproj::barplot_perc_achieve(SDG_network = SDG_network, 
                                   color       = c("#1134A6", "#5EA9A2",  "#228B22", "#1134A6", "#5EA9A2",  "#228B22"), # Mar, Coast, Ter, Mar_neg, Coast_neg, Ter_neg
                                   save        = TRUE)
  
  ## ---- Plot legend for the two plots
  NCSSDGproj::barplot_legend(data_plot = SDG_network[["score_pos"]][["data_pourc"]], 
                             color     = c("#1134A6", "#5EA9A2", "#228B22"))
    
  ## ---- Bind fig 1A with fig 1B
  NCSSDGproj::Figure2(save = TRUE)
  
  
#####################################################################
#                                                                   #
# produce FIGURE 3 - Complementarity of NCS in target's achievement #
#                                                                   #
#####################################################################
rm(list = ls(), envir = .GlobalEnv)
  
  
### ----- Load data 
sheets  <- NCSSDGproj::read_all_sheets()

### ----- Format data 

  ## ---- From sheets to df
  matrix_all <- NCSSDGproj::sheets_to_matrix(sheets_list = sheets)
  
  ## ---- From dataframe to contingency matrixces
  test <- lapply(matrix_all, NCSSDGproj::contingency_mat_targets)

complementarity_net <- bipartite::networklevel(web   = test[[1]], 
                                               index = c("niche overlap", "functional complementarity"),
                                               level = "lower")
  
complementarity_grp <- bipartite::grouplevel(web   = test[[1]], 
                                             index = c("niche overlap", "functional complementarity"),
                                             level = "lower")

bipartite::grouplevel(web   = test[[1]][1:2,], 
                      index = c("niche overlap", "functional complementarity"),
                      level = "lower")

overlap_pos <- spaa::niche.overlap(mat    = t(test[[1]]),
                                   method = "levins")
overlap_pos <- spaa::niche.overlap(mat    = t(test[[1]][1:4]),
                                   method = "levins")

spaa::niche.overlap()


mean(overlap_pos)

overlap_neg <- spaa::niche.overlap(mat    = t(test[[2]]),
                                   method = "levins")
mean(overlap_neg)

