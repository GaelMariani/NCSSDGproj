##################################################
#
# Data and Code for NCS - SDG analysis
#
##################################################

### ----- clean workspace
rm(list = ls())

### ----- install devtools
install.packages("devtools")

### ----- install/update packages
devtools::install_deps()

### ----- load functions in the compendium
devtools::load_all()

  ## ---- in case of any problem with the installation of the package GGally try
  devtools::install_github("ggobi/ggally")


### ----- load rawdata and icons
raw_dat <- NCSSDGproj::read_matrix()
pathSDG <- NCSSDGproj::load_SDG_icon()
pathNCS <- NCSSDGproj::load_NCS_icon()



##################################################
#
### ----- produce FIGURE 2 - Relationships between NCS implementation and SDG achievement
#
##################################################

  ## ---- format icon
  icon_SDG <- NCSSDGproj::format_icons(pathSDG, icon_SDG = TRUE)
  icon_NCS <- NCSSDGproj::format_icons(pathNCS, icon_SDG = FALSE)
  
  ## ---- format data
  data_long <- NCSSDGproj::matrix_to_longDF(matrix01 = raw_dat) # matrix in long format
  SDG_matrix <- NCSSDGproj::matrix_SDG(data_long = data_long) # weighted contingency matrix of SDG
  SDG_net <- NCSSDGproj::matrix_to_network(matrix = SDG_matrix, mode1 = "P" , mode2 = "A") # network object
  data_pour <- NCSSDGproj::perc_SDG(data_long = data_long) # % of target achieved
  
  ## ---- plot panel A - the bipartite network
  NCSSDGproj::plot_network(SDG_net, SDG_matrix,
                           icon_SDG = icon_SDG,icon_NCS = icon_NCS,
                           nodes_col = c(rep("#228B22", 4), rep("#5EA9A2", 4), rep("#1134A6", 3)),
                           save = TRUE)
  
  ## ---- plot panel B - the barplot
  NCSSDGproj::barplot_percSDG(data_plot = data_pour, 
                              color = c("#1134A6", "#5EA9A2", "#228B22"), #mar, coast, terr
                              save = TRUE, 
                              legend = TRUE)
  
  ## ---- bind fig 1A with fig 1B
  NCSSDGproj::Figure2(save = TRUE)
  
  
  
##################################################  
#
### ----- produce FIGURE 3 - Complementarity of NCS in target's achievement
#
################################################## 
    
  ## ---- format data
  matrix01 <- NCSSDGproj::contingency_mat_targets(raw_dat)
  info_NCS <- NCSSDGproj::NCS_info(matrix01)
  axis2_targ <- NCSSDGproj::SDG_contrib_tbl() 
  
  ## ---- correspondance analysis
  contri_CA <- NCSSDGproj::CA_contri_vars(matrix01 = matrix01, 
                                          axis2_targ = axis2_targ,
                                          colNCS_ter = "#228B22", 
                                          colNCS_coast = "#5EA9A2",
                                          colNCS_mar = "#1134A6")
  
  ## ---- Figure with all panels
  NCSSDGproj::Figure3(data = contri_CA[["CorresAna"]],
                      targ_contrib12 = contri_CA[["col_contrib"]][["axe12"]],
                      data_arrow = contri_CA[["data_arrow"]],
                      colNCS_ter = "#228B22", 
                      colNCS_coast = "#5EA9A2",
                      colNCS_mar = "#1134A6",
                      save = TRUE)
  
  
  
##################################################  
#
### ----- null models for Modularity + Nestedness + Insurance
#
##################################################  
  
  ## ---- Modularity and Nestedness - NMalgo = "quasiswap" to conserve marginal sums
  mod_nest_res <- NCSSDGproj::NullModels(matrix01 = matrix01, 
                                         rawdata = raw_dat, 
                                         NMalgo = "quasiswap", 
                                         NESTmethod ="NODF",
                                         Nrun = 10, 
                                         Nsim = 5, # Nsim = 1000 in the paper - TAKES TIME TO RUN
                                         TargetInsurance = FALSE)
  
  ## ---- Insurance - NMalgo = "r00" to not conserve marginal sums
  TUI_TOI_res <- NCSSDGproj::NullModels(matrix01 = matrix01, 
                                        rawdata = raw_dat, 
                                        NMalgo = "r00", 
                                        NESTmethod = "NODF",
                                        Nsim = 100, # Nsim = 1000 in the paper - TAKES LESS TIME TO RUN
                                        TargetInsurance = TRUE)
  
  
  
################################################## 
#
### ----- produce FIGURE 4 - Target's insurance
#
##################################################
  
  ## ---- format data
  null_data <- NCSSDGproj::null_data_CircPlot(matrix01 = matrix01, nsim = 10) # null data to produce red points on the fig
  SDG_info <- NCSSDGproj::SDG_infos(matrix01 = matrix01)
  NCS_info <- NCSSDGproj::NCS_info(matrix01 = matrix01)
  NCSSDGproj::legend_verti(data_plot = data_pour, color = c("#1134A6", "#5EA9A2", "#228B22")) # produce legend
  
  
  
  ## ---- compute target's insurance
  data_Insurance <- NCSSDGproj::Insurance_data2plot(matrix01 = matrix01, Ntarget = ncol(matrix01)) 
  data_circu <- NCSSDGproj::circular_data_Insurance(data_Insurance = data_Insurance, 
                                                    data_long = data_long, 
                                                    SDG_info = SDG_info, 
                                                    NCS_info = NCS_info) # format data with polar coordinates
  

  
  ## ---- plot 
  NCSSDGproj::circular_plot_Insurance(data = data_circu[[1]], 
                                      label_data = data_circu[[2]],
                                      base_data = data_circu[[3]],
                                      grid_data = data_circu[[4]],
                                      SDG_info = SDG_info,
                                      colNCS_ter = "#228B22", 
                                      colNCS_coast = "#5EA9A2",
                                      colNCS_mar = "#1134A6",
                                      iconSDG = icon_SDG,
                                      save = TRUE)
  
  
  
  
  
  
  
  
  
  