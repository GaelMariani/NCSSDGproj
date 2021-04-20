### Bray-Curtis values
rm(list = ls(), envir = .GlobalEnv)   


### ----- LOAD DATA

  ## ---- Data of links between NCS and SDG
  sheets  <- NCSSDGproj::read_all_sheets()
  
### ----- FORMAT DATA

  ## ---- From sheets to df
  matrix_all <- NCSSDGproj::sheets_to_matrix(sheets_list = sheets, binary = FALSE)
  
  ## ---- From dataframes to contingency matrices 
  matrix_conting_bin <- lapply(matrix_all, NCSSDGproj::contingency_mat_targets, binary = FALSE)

### ----- ANALYSIS
  
  ## ---- Compute Bray-Curtis distance
  BC_dist <- vegan::vegdist(x      = matrix_conting_bin[["score_pos"]],
                            method = "bray",
                            binary = FALSE)
  
  BC_dist_mat <- as.matrix(BC_dist)
  
  ## ---- PCoA
  pcoa <- ape::pcoa(D = BC_dist)
  
### ----- PLOT 
  
  ## Biplot
  barplot(pcoa$values$Rel_corr_eig)
  ape::biplot.pcoa(pcoa)
  
  ## Heatmap
  heatmap(BC_dist_mat)
  corrplot::corrplot(BC_dist_mat, type = "lower", is.corr = FALSE)
  
### Chi²

  
  
  