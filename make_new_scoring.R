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


### ----- load rawdata and icons
sheets <- NCSSDGproj::read_all_sheets()
pathSDG <- NCSSDGproj::load_SDG_icon()
pathNCS <- NCSSDGproj::load_NCS_icon()


###################################################################################
#                                                                                 #
# produce FIGURE 2 - Relationships between NCS implementation and SDG achievement #
#                                                                                 #
###################################################################################

### ---- format icon
icon_SDG <- NCSSDGproj::format_icons(pathSDG, icon_SDG = TRUE)
icon_NCS <- NCSSDGproj::format_icons(pathNCS, icon_SDG = FALSE)
  
### ---- format data
matrix_all <- NCSSDGproj::sheets_to_matrix(sheets_list = sheets)

SDG_network <- lapply(1:length(matrix_all), 
                      function(i){
                        
                        # -- matrix in long format
                        data <- NCSSDGproj::matrix_to_longDF(matrix01 = matrix_all[[i]])
                        
                        # -- weighted contingency matrix of SDG
                        SDG_matrix <- NCSSDGproj::matrix_SDG(data_long = data)
                        
                        # -- create a network object
                        SDG_network <- NCSSDGproj::matrix_to_network(matrix = SDG_matrix,
                                                                     mode1  = "P",
                                                                     mode2  = "A")
                        
                        # -- percentage of target achieved
                        data_pourc <- NCSSDGproj::perc_SDG(data_long = data)
                        
                        return(list(matrix = SDG_matrix, network = SDG_network, data_pourc = data_pourc))
                        
                      })

names(SDG_network) <- names(matrix_all)

### ---- Plot data

  ## --- cumulated data

    # -- plot panel A - the bipartite network
    NCSSDGproj::plot_network(network_obj = SDG_network[["score_cumulate"]][["network"]],
                             matrix      = SDG_network[["score_cumulate"]][["matrix"]],
                             icon_SDG    = icon_SDG,
                             icon_NCS    = icon_NCS,
                             nodes_col   = c(rep("#228B22", 4), rep("#5EA9A2", 4), rep("#1134A6", 3)),
                             save        = FALSE)
    
    # -- plot panel B - the barplot
    NCSSDGproj::barplot_percSDG(data_plot = SDG_network[["score_pos"]][["data_pourc"]], 
                                color     = c("#1134A6", "#5EA9A2", "#228B22"), #mar, coast, terr
                                save      = FALSE, 
                                legend    = FALSE)
    
  
  ## --- negative matrix 
    data_neg <- NCSSDGproj::matrix_to_longDF(matrix01 = matrix_all[["score_neg"]])
  
    # --- net matrix
    data_net <- NCSSDGproj::matrix_to_longDF(matrix01 = matrix_all[["score_net"]])
  
  ## ---- format negative data

