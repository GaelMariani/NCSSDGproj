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

### ----- LOAD DATA (rawdata and icons)
sheets  <- NCSSDGproj::read_all_sheets()
pathSDG <- NCSSDGproj::load_SDG_icon()
pathNCS <- NCSSDGproj::load_NCS_icon()


### ----- FORMAT ICONS (NCS and SDG icons)
icon_SDG <- NCSSDGproj::format_icons(pathSDG, icon_SDG = TRUE)
icon_NCS <- NCSSDGproj::format_icons(pathNCS, icon_SDG = FALSE)
  

### ----- FORMAT DATA
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


### ----- PLOT DATA

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
  
  
### ----- LOAD DATA
  
  ## ---- Data of links between NCS and SDG
  sheets  <- NCSSDGproj::read_all_sheets()

### ----- FORMAT DATA

  ## ---- From sheets to df
  matrix_all <- NCSSDGproj::sheets_to_matrix(sheets_list = sheets)
  
  ## ---- From dataframes to contingency matrices 
  
    # --- binary data
    matrix_conting_bin <- lapply(matrix_all, NCSSDGproj::contingency_mat_targets, binary = TRUE)
    
    # --- 0-1-2 score
    matrix_conting <- lapply(matrix_all, NCSSDGproj::contingency_mat_targets, binary = FALSE)
  
  ## ---- Informations on the NCS
  info_NCS <- NCSSDGproj::NCS_info(matrix_cont = matrix_conting_bin[[1]])
  
  ## ---- Table of the most contributing targets
  axis2_targ <- NCSSDGproj::SDG_contrib_tbl() 
  
### ----- ANALYSIS  
  
  ## ---- Binary analysis (CHi2 distance and correspondance analysis)
  
    # ---- Correspondance analysis for POSITIVE links with the contribution of each target and NCS to the variance of each axis
    contri_CA_pos <- NCSSDGproj::CA_contri_vars(matrix_cont  = matrix_conting_bin[["score_pos"]],
                                                axis2_targ   = axis2_targ,
                                                colNCS_ter   = "#228B22",
                                                colNCS_coast = "#5EA9A2",
                                                colNCS_mar   = "#1134A6")
    
    # ---- Correspondance analysis for NEGATIVE links with the contribution of each target and NCS to the variance of each axis
    contri_CA_neg <- NCSSDGproj::CA_contri_vars(matrix_cont  = matrix_conting_bin[["score_neg"]],
                                                axis2_targ   = axis2_targ,
                                                colNCS_ter   = "#228B22",
                                                colNCS_coast = "#5EA9A2",
                                                colNCS_mar   = "#1134A6")
    
  ## ---- Non binary analysis (Bray-Curtis distance and PCoA)
    
    
    
    
### PLOT DATA 
  
  ## ---- Figure with all panels for POSITIVE links
  NCSSDGproj::Figure3(data           = contri_CA_pos[["CorresAna"]],
                      targ_contrib12 = contri_CA_pos[["col_contrib"]][["axe12"]],
                      data_arrow     = contri_CA_pos[["data_arrow"]],
                      colNCS_ter     = "#228B22", 
                      colNCS_coast   = "#5EA9A2",
                      colNCS_mar     = "#1134A6",
                      save           = TRUE,
                      name           = "Figure3_pos")
  
  ## ---- Figure with all panels for NEGATIVE links
  NCSSDGproj::Figure3(data           = contri_CA_neg[["CorresAna"]],
                      targ_contrib12 = contri_CA_neg[["col_contrib"]][["axe12"]],
                      data_arrow     = contri_CA_neg[["data_arrow"]],
                      colNCS_ter     = "#228B22", 
                      colNCS_coast   = "#5EA9A2",
                      colNCS_mar     = "#1134A6",
                      save           = TRUE,
                      name           = "Figure3_neg")
  
  

#####################################################################
#                                                                   #
#        Null models for Modularity + Nestedness + Insurance        #
#                                                                   #
#####################################################################  
rm(list = ls(), envir = .GlobalEnv)
  
### ----- LOAD DATA
  
  ## ---- Data of links between NCS and SDG
  sheets  <- NCSSDGproj::read_all_sheets()
  
### ----- FORMAT DATA
  
  ## ---- From sheets to df
  matrix_all <- NCSSDGproj::sheets_to_matrix(sheets_list = sheets)
  
  ## ---- From dataframes to contingency matrices 
  matrix_conting_bin <- lapply(matrix_all, NCSSDGproj::contingency_mat_targets, binary = TRUE)
    

### ----- ANALYSIS
  
  ## ---- Modularity and Nestedness - NMalgo = "quasiswap" to conserve marginal sums
  
    # --- POSITIVE scores
    mod_nest_res_pos <- NCSSDGproj::NullModels(matrix01        = matrix_conting_bin[["score_pos"]], 
                                               rawdata         = matrix_all[["score_pos"]], 
                                               NMalgo          = "quasiswap", 
                                               NESTmethod      = "NODF",
                                               Nrun            = 1, 
                                               Nsim            = 99, # Nsim = 1000 in the paper - TAKES TIME TO RUN
                                               TargetInsurance = FALSE,
                                               save = TRUE,
                                               name = "Nest_Modu_res_pos")
    
    # --- NEGATIVE scores
    mod_nest_res_neg <- NCSSDGproj::NullModels(matrix01        = matrix_conting_bin[["score_neg"]], 
                                               rawdata         = matrix_all[["score_neg"]], 
                                               NMalgo          = "quasiswap", 
                                               NESTmethod      = "NODF",
                                               Nrun            = 1, 
                                               Nsim            = 99, # Nsim = 1000 in the paper - TAKES TIME TO RUN
                                               TargetInsurance = FALSE,
                                               save = TRUE,
                                               name = "Nest_Modu_res_neg")
    
    
    
    
    
  

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

