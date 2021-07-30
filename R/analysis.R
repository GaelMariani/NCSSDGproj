#' Target Under Insurance -TUI- And Over Insurance -TOI-
#' Functional indices adapted for Mouillot et al. 2014 - Functional over-redundancy and high functional vulnerability in global fish faunas on tropical reefs - https://doi.org/10.1073/pnas.1317625111
#'
#' @param data_TI a dataframe with the number of time each target is achieved
#' @param Necosystem the number of ecosystem
#' @param Ntarget the number of targets
#'
#' @return a three columns dataframe with TUI, TI and TOI values 
#' @export
#'
#' @examples
TUI_TOI <- function(data_TI, Necosystem, Ntarget)  {
  
  
  # TUI, percentage of target achieved by only one ecosystem - equivalent to Functional Vulnerability in Mouillot et al. (2014)
  for(nrow in 1:nrow(data_TI)){
    data_TI$min[nrow] <- min(data_TI$value[nrow]-1, 1)
  }
  
  target_under_insurance <- (Ntarget - sum(data_TI$min))/Ntarget
  
  # Target Insurance the mean number of ecosystem that help in achieving a target - equivalent to Function redundancy in Mouillot et al. (2014)
  Target_insurance <- sum(data_TI$value)/Ntarget  
  
  
  # TOI, percentage of NCS in excess in target having more NCS than expected from target insurance - equivalent to Functional Over Redundancy in Mouillot et al. (2014)
  for(nrow in 1:nrow(data_TI)){
    data_TI$max[nrow] <- max(data_TI$value[nrow], Target_insurance)
  }
  
  target_over_insurance <- (sum(data_TI$max - Target_insurance))/sum(data_TI$value)
  
   
  TUI_TOI_TI <- data.frame(TUI = target_under_insurance,
                           TI  = Target_insurance,
                           TOI = target_over_insurance)
  
  
  return(TUI_TOI_TI)
}


#' SES Pvalue Calculation
#'
#' @param val_obs observed value of the indice
#' @param mean_null average value of the indice computed on null matrices 
#' @param sd_null sd value of the indice computed on null matrices 
#' @param rowname a character specifying rowname
#'
#' @return return a 3 columns dataframe with null model results, SES and pvalues
#' @export
#'
#' @examples
SES_pval <- function(val_obs, mean_null, sd_null, rowname) {
  
  results_nm <- data.frame(Val_Obs   = val_obs,
                           Val_nulls =  mean_null,
                           SES       = (val_obs - mean_null)/sd_null,
                           pvalue    = 2*pnorm(-abs((val_obs - mean_null)/sd_null)))
  
  rownames(results_nm) <- rowname
  
  return(results_nm)
            
}


#' Null Model Analysis
#'
#' @param matrix01 formatted matrix to calculate network indices, nestedness and modularity - use contingency_mat_targets
#' @param rawdata a dataframe with targets of the SDGs in columns and NCSs in rows - use sheets_to_df
#' @param NMalgo name of null model algorithm 
#' @param NESTmethod method to calculate nestedness, use "NODF" or "weighted NODF"
#' @param Nrun 	Number of replicate runs for metaComputesModules 
#' @param Nsim Number of null matrices
#' @param TargetInsurance if TRUE, compute Insurance indices, if FALSE calculate  network indices
#' @param save if statement to save the results
#' @param name the name of the data to be saved
#'
#' @return a 3 column dataframe with null model results for nestedness and modularity OR TOi and TUI
#' @export
#'
#' @examples
NullModels <- function(matrix01, rawdata, NMalgo, NESTmethod, Nrun, Nsim, TargetInsurance = FALSE, save, name) {
  
  ## Compute NESTEDNESS and MODULARITY
  if(TargetInsurance == FALSE){
    
    # Observed nestedness and modularity
    modularity_obs <- bipartite::metaComputeModules(matrix01, N = Nrun)
    nestedness_obs <- bipartite::nested(matrix01, NESTmethod)

    # Null modularity and nestedness 
    null_matrices <- stats::simulate(vegan::nullmodel(matrix01, NMalgo), nsim = Nsim)
    null_matrices <- asplit(null_matrices, 3)
    
    mod_null <- sapply(null_matrices, bipartite::metaComputeModules, Nrun) # modularity
    mod_null <- sapply(mod_null, function(x) x@likelihood)
    
    nest_null <- sapply(null_matrices, bipartite::nested, method = NESTmethod)
    
  ## Statistic tests 
    
    # For modularity >> Test the gaussian distribution of the 999 null values (shapiro test), if gaussian, calculate SES
    shapiro_mod  <- shapiro.test(mod_null)$p.value
    
    if(shapiro_mod < 0.05){
      cat("Shapiro test, Modularity p.value:", signif(shapiro_mod, digits = 3), 
          "\n The 999 null values does not follow a normal distribution\n",
          ">>> You can't calculate Standardize Effect Size \n \n",
          "2.5th and 97.5 Percentiles modularity: \n",
          quantile(mod_null, c(0.025, 0.975)),
          "\n Modularity observed:", modularity_obs@likelihood)
      
      mod_res <- data.frame(Observed_val         = modularity_obs@likelihood,
                            Mean_null_val        = mean(mod_null),
                            Percentiles_2.5_97.5 = paste0("[",round(quantile(mod_null, c(0.025, 0.975))[1], 4), " ; ", round(quantile(mod_null, c(0.025, 0.975))[2], 4), "]"))
      rownames(mod_res) <- "Modularity"
      
    } else {
      cat("Shapiro test, Modularity p.value:", round(shapiro_mod, digits = 5),"\n The 999 null values follow a normal distribution\n", ">>> You can calculate Standardize Effect Size")
      
      # Store results into a data frame and compute SES and pvals ==> 2*pnorm for bilateral test, if gaussian, calculate SES
      mod_res <- NCSSDGproj::SES_pval(val_obs = modularity_obs@likelihood, mean_null = mean(mod_null), sd_null = sd(mod_null), rowname = "Modularity")
      mod_res
    }
  
    # For nestedness >> Test the gaussian distribution of the 999 null values (shapiro test)
    shapiro_nest <- shapiro.test(nest_null)$p.value
    
    if(shapiro_nest < 0.05){
      cat("Shapiro test, Nestedness p.value:", signif(shapiro_nest, digits = 3),
          "\n The 999 null values does not follow a normal distribution\n", 
          ">>> You can't calculate Standardize Effect Size \n \n",
          "2.5th and 97.5 Percentiles nestedness: \n",
          quantile(nest_null, c(0.025, 0.975)),
          "\n Nestedness observed:", nestedness_obs)
      
      nest_res <- data.frame(Observed_val         = nestedness_obs,
                             Mean_null_val        = mean(nest_null),
                             Percentiles_2.5_97.5 = paste0("[",round(quantile(nest_null, c(0.025, 0.975))[1], 2), " ; ", round(quantile(nest_null, c(0.025, 0.975))[2], 2), "]"))
      rownames(nest_res) <- "Nestedness"
      
    } else {
      cat("Shapiro test, Nestedness p.value:", round(shapiro_nest, digits = 5),"\n The 999 null values follow a normal distribution\n", ">>> You can calculate Standardize Effect Size")
      
      # Store results into a data frame and compute SES and pvals ==> 2*pnorm for bilateral test
      nest_res <- NCSSDGproj::SES_pval(val_obs = nestedness_obs, mean_null = mean(nest_null), sd_null = sd(nest_null), rowname = "Nestedness")
      nest_res
    }
      
    ## Produce and save normality plots
    hist_mod <- ggplot2::ggplot(as.data.frame(mod_null), ggplot2::aes(x = mod_null)) + ggplot2::xlab("null values") + ggplot2::geom_histogram(color = "black", fill = "grey80") + ggplot2::annotate(geom = "text", x = Inf, y = Inf, vjust = 1.2, hjust = 1.05, size = 3, label = paste("Shapiro test, p.value = ", signif(shapiro_mod, 3))) + ggplot2::ggtitle("Modularity") + ggplot2::theme_bw() +  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) 
    hist_nest <- ggplot2::ggplot(as.data.frame(nest_null), ggplot2::aes(x = nest_null)) + ggplot2::xlab("null values") + ggplot2::geom_histogram(color = "black", fill = "grey80") + ggplot2::annotate(geom = "text", x = -Inf, y = Inf, vjust = 1.2, hjust = -0.02, size = 3, label = paste("Shapiro test, p.value = ", signif(shapiro_nest, 3))) + ggplot2::ggtitle("Nestedness") + ggplot2::theme_bw() +  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) 
    qqplot_mod <- ggpubr::ggqqplot(mod_null) + ggplot2::theme_bw() + ggplot2::annotate(geom = "text", x = -Inf, y = Inf, vjust = 1.2, hjust = -0.02, size = 3, label = paste("Shapiro test, p.value = ", signif(shapiro_mod, 3)))
    qqplot_nest <- ggpubr::ggqqplot(nest_null) + ggplot2::theme_bw() + ggplot2::annotate(geom = "text", x = -Inf, y = Inf, vjust = 1.2, hjust = -0.02, size = 3, label = paste("Shapiro test, p.value = ", signif(shapiro_nest, 3)))
    qqplots <- (hist_mod | hist_nest)/(qqplot_mod | qqplot_nest)
    ggplot2::ggsave(here::here("figures", "Supp_fig5.png"), qqplots, width = 9, height = 5, device = "png")
    save(qqplots, file = here::here("results", "Supp_fig5.RData"))

      
  ## Bind and store data
  mod_nest_res <- rbind(mod_res, nest_res)
  res <- mod_nest_res

    
  } # end IF
  
  ### Compute Target Insurance values     
  else{
    
    ## Observed data
    
      # Format data to have the times a target is achieved 
      TI_data_obs <- rawdata %>%
        tidyr::gather(., target, value, -1) %>%
        stats::setNames(c("Ecosystem", "target", "value")) %>%
        replace(., . < 0, 0) %>%
        dplyr::group_by(target) %>% 
        dplyr::summarise(value = sum(value)) %>%
        dplyr::filter(value != 0)
      
      # Calculate TOI and TUI on observed data
      indices_obs <- NCSSDGproj::TUI_TOI(TI_data_obs, Necosystem = 11, Ntarget = nrow(TI_data_obs))
    
    ## Null matrices 
    nm_r00 <- stats::simulate(vegan::nullmodel(matrix01, NMalgo), nsim = Nsim) # Returns a list of matrices
    nm_r00 <- asplit(nm_r00, 3)
    
      # Format data to calculate indices
      data_TI <- lapply(nm_r00, NCSSDGproj::data_TI)
      
      # Calculate TOI and TUI on null matrices
      indices_NM <- lapply(data_TI, NCSSDGproj::TUI_TOI, Necosystem = 11, Ntarget = nrow(TI_data_obs))
      indices_nullmod_df <- do.call(rbind, indices_NM)
      
    ## Statistic tests >> Test the gaussian distribution of the 999 null values (shapiro test)
      
      # For TUI
      shapiro_TUI <- shapiro.test(indices_nullmod_df$TUI)$p.value
        
      if(shapiro_TUI < 0.05){
        cat("Shapiro test, TUI p.value:", signif(shapiro_TUI, digits = 3),
            "\n The 999 null values does not follow a normal distribution\n", 
            ">>> You can't calculate Standardize Effect Size \n \n",
            "2.5th and 97.5 Percentiles nestedness: \n",
            quantile(indices_nullmod_df$TUI, c(0.025, 0.975)),
            "\n TUI observed:", indices_obs$TUI)
        
        TUI_res <- data.frame(Observed_val         = indices_obs$TUI,
                              Mean_null_val        = mean(indices_nullmod_df$TUI),
                              Percentiles_2.5_97.5 = paste0("[",round(quantile(indices_nullmod_df$TUI, c(0.025, 0.975))[1], 3), " ; ", round(quantile(indices_nullmod_df$TUI, c(0.025, 0.975))[2], 3), "]"))
        rownames(TUI_res) <- "TUI"
          
      } else {
        cat("Shapiro test, TUI p.value:", round(shapiro_TUI, digits = 5),"\n The 999 null values follow a normal distribution\n", ">>> You can calculate Standardize Effect Size")
        
          # Store results into a data frame and compute SES and pvals ==> 2*pnorm for bilateral test
          TUI_res <- NCSSDGproj::SES_pval(val_obs = indices_obs$TUI, mean_null = mean(indices_nullmod_df$TUI), sd_null = sd(indices_nullmod_df$TUI), rowname = "TUI") 
      }  
      
      # For TOI
      shapiro_TOI <- shapiro.test(indices_nullmod_df$TOI)$p.value
      
      if(shapiro_TOI < 0.05){
        cat("Shapiro test, TOI p.value:", signif(shapiro_TOI, digits = 3),
            "\n The 999 null values does not follow a normal distribution\n", 
            ">>> You can't calculate Standardize Effect Size \n \n",
            "2.5th and 97.5 Percentiles nestedness: \n",
            quantile(indices_nullmod_df$TOI, c(0.025, 0.975)),
            "\n TOI observed:", indices_obs$TOI)
        
        TOI_res <- data.frame(Observed_val         = indices_obs$TOI,
                              Mean_null_val        = mean(indices_nullmod_df$TOI),
                              Percentiles_2.5_97.5 = paste0("[",round(quantile(indices_nullmod_df$TOI, c(0.025, 0.975))[1], 3), " ; ", round(quantile(indices_nullmod_df$TUI, c(0.025, 0.975))[2], 3), "]"))
        rownames(TOI_res) <- "TOI"
        
      } else {
        cat("Shapiro test, TOI p.value:", round(shapiro_TOI, digits = 3),"\n The 999 null values follow a normal distribution\n", ">>> You can calculate Standardize Effect Size")
        
        # Store results into a data frame and compute SES and pvals ==> 2*pnorm for bilateral test
        TOI_res <- NCSSDGproj::SES_pval(val_obs = indices_obs$TOI, mean_null = mean(indices_nullmod_df$TOI), sd_null = sd(indices_nullmod_df$TOI), rowname = "TUI") 
      }  
      
      
      ## Produce and save normality plots
      hist_TUI <- ggplot2::ggplot(indices_nullmod_df, ggplot2::aes(x = TUI)) + ggplot2::xlab("null values") + ggplot2::geom_histogram(color = "black", fill = "grey80") + ggplot2::annotate(geom = "text", x = Inf, y = Inf, vjust = 1.2, hjust = 1.05, size = 3, label = paste("Shapiro test, p.value = ", signif(shapiro_TUI, 3))) + ggplot2::ggtitle("TUI") + ggplot2::theme_bw() +  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) 
      hist_TOI <- ggplot2::ggplot(indices_nullmod_df, ggplot2::aes(x = TOI)) + ggplot2::xlab("null values") + ggplot2::geom_histogram(color = "black", fill = "grey80") + ggplot2::annotate(geom = "text", x = -Inf, y = Inf, vjust = 1.2, hjust = -0.02, size = 3, label = paste("Shapiro test, p.value = ", signif(shapiro_TOI, 3))) + ggplot2::ggtitle("TOI") + ggplot2::theme_bw() +  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) 
      qqplot_TUI <- ggpubr::ggqqplot(indices_nullmod_df$TUI) + ggplot2::theme_bw() + ggplot2::annotate(geom = "text", x = -Inf, y = Inf, vjust = 1.2, hjust = -0.02, size = 3, label = paste("Shapiro test, p.value = ", signif(shapiro_TUI, 3)))
      qqplot_TOI <- ggpubr::ggqqplot(indices_nullmod_df$TOI) + ggplot2::theme_bw() + ggplot2::annotate(geom = "text", x = -Inf, y = Inf, vjust = 1.2, hjust = -0.02, size = 3, label = paste("Shapiro test, p.value = ", signif(shapiro_TOI, 3)))
      qqplots <- (hist_TUI | hist_TOI)/(qqplot_TUI | qqplot_TOI)
      ggplot2::ggsave(here::here("figures", "Supp_fig6.png"), qqplots, width = 9, height = 5, device = "png")
      save(qqplots, file = here::here("results", "Supp_fig6.RData"))
      
      
      # Bind results
      res <- list("TUI" = TUI_res, "TOI" = TOI_res)
      
  } # end ELSE
  
  
  if(save == TRUE) {
    
    save(res, file = here::here("results", paste0(name, ".RData")))
    
  }
  
  return(res)
  
}


#' Correspondance Analysis Of Variable Contribution
#' 
#' @param matrix_cont a matrix with NCS in rows and SDG targets in columns - use contingency_mat_targets
#' @param colNCS_ter color for terrestrial ecosystems
#' @param colNCS_coast color for coastal ecosystems
#' @param colNCS_mar color for marine ecosystems
#'
#' @return a list of three files, with CA analysis results, contribution of row to the variance and the data to add arrow on the plot
#' @export
#'
#' @examples
CA_contri_vars <- function(matrix_cont, colNCS_ter, colNCS_coast, colNCS_mar){
  
  ### Change variable name
  tmp <- data.frame(Ecosystem = rownames(matrix_cont)) %>%
    dplyr::mutate(acronyme  = dplyr::case_when(Ecosystem == "Urban forest"     ~ "(UFo)",
                                               Ecosystem == "Forest"           ~ "(FO)",
                                               Ecosystem == "Peatland"         ~ "(PL)",
                                               Ecosystem == "Grassland"        ~ "(GL)",
                                               Ecosystem == "Mangrove"         ~ "(MG)",
                                               Ecosystem == "Tidalmarsh"       ~ "(TD)",
                                               Ecosystem == "Macroalgae"       ~ "(MA)",
                                               Ecosystem == "Seagrass"         ~ "(SG)",
                                               Ecosystem == "Pelagic area"     ~ "(Pel)",
                                               Ecosystem == "Antarctic"        ~ "(Ant)",
                                               Ecosystem == "Mesopelagic area" ~ "(MP)",
                                               Ecosystem == "Seabed"           ~ "(SB)"),
                  Ecosystem = paste(Ecosystem, acronyme))
  
  grp <- NCSSDGproj::NCS_info(matrix_cont) 
  rownames(matrix_cont) <- tmp$Ecosystem
  
  ### Correspondance Analysis on the matrix
  res.ca <- FactoMineR::CA(matrix_cont, graph = FALSE)
  res.ca[["grp"]] <- grp
  
  ### Contribution of rows (NCS) to the variance of each axis
  row_contrib <- as.data.frame(factoextra::get_ca_row(res.ca)[["contrib"]])
  
    ## select rownames of the most contributing targets (those with a contribution higher than expected)
    row_expect_contrib <- 100/nrow(matrix_cont)
    
      # 1st axis 
      name1_r <- rownames(row_contrib[row_contrib[,1] >= row_expect_contrib,])
      # 2nd axis
      name2_r <- rownames(row_contrib[row_contrib[,2] >= row_expect_contrib,])
      
      row_names12 <- unique(c(name1_r, name2_r))
   
  ### Add a row because seagrass do not have negative interactions.    
  # if(nrow(res.ca[["row"]][["coord"]]) == 11){
  #   
  #   tmp <- res.ca[["row"]][["coord"]]
  #   
  #   new_mat <- matrix(NA, nrow = 11, ncol = ncol(tmp), dimnames = list(c("Urban forest (UFo)", "Forest (FO)", "Peatland (PT)", "Grassland (GL)", "Seagrass(SG)", "Mangrove (MG)", "Tidalmarsh (TD)", "Macroalgae (MA)", "Pelagic area (Pel)", "Antarctic (Ant)", "Mesopelagic area (MP)"),
  #                                                                      dimnames(tmp)[[2]]))
  #   new_mat[-5,] <- tmp
  #   
  #   res.ca[["row"]][["coord"]] <- new_mat
  # }
      
  ### Format data to draw arrows on plot
  data_arrow <- data.frame(y     = c(rep(1.02, 4), 0.95, 0.95),
                           ymax  = c(rep(1.02, 4), 0.95, 0.95),
                           x     = c(min(res.ca[["row"]][["coord"]][9:12, "Dim 1"]),
                                     max(res.ca[["row"]][["coord"]][9:12, "Dim 1"]),
                                     min(res.ca[["row"]][["coord"]][5:8, "Dim 1"], na.rm = TRUE),
                                     max(res.ca[["row"]][["coord"]][5:8, "Dim 1"], na.rm = TRUE),
                                     min(res.ca[["row"]][["coord"]][1:4, "Dim 1"]),
                                     max(res.ca[["row"]][["coord"]][1:4, "Dim 1"])),
                               
                           xmax  = c(max(res.ca[["row"]][["coord"]][9:12, "Dim 1"]),
                                     min(res.ca[["row"]][["coord"]][9:12, "Dim 1"]),
                                     max(res.ca[["row"]][["coord"]][5:8, "Dim 1"], na.rm = TRUE),
                                     min(res.ca[["row"]][["coord"]][5:8, "Dim 1"], na.rm = TRUE),
                                     max(res.ca[["row"]][["coord"]][1:4, "Dim 1"]),
                                     min(res.ca[["row"]][["coord"]][1:4, "Dim 1"])),
                           color = c(rep(colNCS_mar, 2), rep(colNCS_coast, 2), rep(colNCS_ter, 2)),
                           text  = c(rep("Marine NCS", 2), rep("Coastal NCS", 2), rep("Terrestrial NCS", 2)))
  
  ### Remove the sup row added above 
  if(sum(is.na(res.ca[["row"]][["coord"]])) > 0){
    res.ca[["row"]][["coord"]] <-  res.ca[["row"]][["coord"]][-5,]
  }
  

  ### Save data
  CA_contrib <- list("CorresAna"        = res.ca,
                     # "TOP20_axis1_targ" = TOP20_axis1,
                     # "TOP20_axis2_targ" = TOP20_axis2,
                     # "col_contrib"      = list("tot"   = col_contrib, 
                     #                           "axe12" = col_names12, 
                     #                           "axe34" = col_names34),
                     "row_contrib"      = list("tot"   = row_contrib,
                                               "axe12" = row_names12),
                     "data_arrow"       = data_arrow)
  
  return(CA_contrib)

}      

      
#' Randomly Turn Values In A Matrix
#' 
#' Turns x% of 1 into 2 and y% 1 into 0 to mimick expert's desagreement.
#'
#' @param data_links a dataframe with links bewteen targets -in columns- and NCS -in rows-, use sheets_to_df
#' @param percentage percentage -from 0 to 1- of values that match to be replaced
#' @param binary if statement to turn all values 2 into values 1 for binary analysis
#'
#' @return a dataframe with modified values for a given number of matrices
#' @export
#'
#' @examples
turn_values_randomly <- function(data_links, percentage, binary = TRUE){
  
  ### Function to replace randomly wanted values
  turn_values <- function(data_links, value_to_match, new_value, perc){
  
    ## Turn df into a matrix
    matrix <- as.matrix(data_links[, -1])
    
    ## Select a percentage of values == value_to_match and replace it by new_value 
    
      # Cell numbers for which value == value_to_match
      cell_number <- which(matrix == value_to_match)
      
      # Sample randomly a percentage of matching values
      vals_to_be_modified <- round(sample(x       = 1:length(cell_number), 
                                          size    = round(x      = perc*length(cell_number),
                                                          digits = 0),
                                          replace = FALSE))
      
      
      # Replace original values by the new ones
      matrix[cell_number[vals_to_be_modified]] <- new_value
      
    ## Format data
    data <- as.data.frame(matrix) %>%
      cbind(data_links[,1], .)
    
    colnames(data)[1] <- "ecosystem"
    
    return(data)
  
  }
  
  ### Apply the function
    
    ## Turn x% of 1 into 2: mimicking that we missed references at the global scale in 10% of cases
    data_1st_modif <- turn_values(data_links     = data_links, 
                                  value_to_match = 1, 
                                  new_value      = 2, 
                                  perc           = percentage)
    
    ## Turn x% of 1 into 0: mimicking a disagreement between expert on 10% of targets scored with a one
    data_2nd_modif <- turn_values(data_links     = data_1st_modif, 
                                  value_to_match = 1, 
                                  new_value      = 0, 
                                  perc           = percentage)
    
  ### Choose if return binary data (all 2 transformed into 1) or data from 0 to 2
  if(binary == TRUE){
    data_2nd_modif[data_2nd_modif == 2] <- 1
  }
    
  return(data_2nd_modif)
  
}


#' Run Sensitivity Analysis
#'
#' @param matrix_rep a list of matrix, use contingency_mat_targets
#' @param obs_values observed metric 
#' @param Nrun Number of replicate runs for metaComputesModules 
#' @param save if statement to save the results
#' @param name the name of the data to be saved
#'
#' @return a dataframe with observed and modify metrics and SES and pvalue
#' @export
#'
#' @examples
sensitivity_analysis <- function(matrix_rep, obs_values, Nrun, save = TRUE, name){
  
  ### Modularity
  modularity <- sapply(matrix_rep, bipartite::metaComputeModules, Nrun)
  modularity_vals <- sapply(modularity, function(x) x@likelihood)
  
  
  ### Nestedness
  nestedness <- sapply(matrix_rep, bipartite::nested, method = "NODF")
  
  
  ### Insurance
    
    ## Format data
    data_TI <- lapply(matrix_rep, NCSSDGproj::data_TI)
    
    ## Calculations
    insurance <- lapply(1:length(data_TI), 
                        function(i){
                          res <- NCSSDGproj::TUI_TOI(data_TI    = data_TI[[i]],
                                                     Necosystem = 11, 
                                                     Ntarget    = nrow(data_TI[[i]]))
                        })
    
    insurance_vals <- do.call(rbind, insurance)
    
  ### Statistic tests
    
    ## For modularity and nestedness
    shapiro_mod <- shapiro.test(modularity_vals)$p.value
    shapiro_nest <- shapiro.test(nestedness)$p.value
    
      # Choice between SES computation and 2.5 and 07.5 percentiles
        
        # Modularity
        if(shapiro_mod < 0.05){
          cat("Shapiro test, Modularity p.value:", signif(shapiro_mod, digits = 3), 
              "\n The 999 null values does not follow a normal distribution\n",
              ">>> You can't calculate Standardize Effect Size \n \n",
              "2.5th and 97.5 Percentiles modularity: \n",
              quantile(modularity_vals, c(0.025, 0.975)),
              "\n Modularity observed:", obs_values["Modularity", "Val_Obs"])
          
          mod_res <- data.frame(Observed_val         = obs_values["Modularity", "Val_Obs"],
                                Mean_null_val        = mean(modularity_vals),
                                Percentiles_2.5_97.5 = paste0("[",round(quantile(mod_null, c(0.025, 0.975))[1], 4), " ; ", round(quantile(mod_null, c(0.025, 0.975))[2], 4), "]"))
          rownames(mod_res) <- "Modularity"
          
        } else {
          cat("Shapiro test, Modularity p.value:", round(shapiro_mod, digits = 5),"\n The 999 null values follow a normal distribution\n", ">>> You can calculate Standardize Effect Size")
          
          # Store results into a data frame and compute SES and pvals ==> 2*pnorm for bilateral test, if gaussian, calculate SES
          mod_res <- NCSSDGproj::SES_pval(val_obs   = obs_values["Modularity", "Val_Obs"], mean_null = mean(modularity_vals), sd_null = sd(modularity_vals), rowname   = "Modularity")
        }
    
    
        # Nestedness
        if(shapiro_nest < 0.05){
          cat("Shapiro test, Nestedness p.value:", signif(shapiro_nest, digits = 5),
              "\n The 999 null values does not follow a normal distribution\n", 
              ">>> You can't calculate Standardize Effect Size \n \n",
              "2.5th and 97.5 Percentiles nestedness: \n",
              quantile(nestedness, c(0.025, 0.975)),
              "\n Nestedness observed:", obs_values["Nestedness", "Val_Obs"])
          
          nest_res <- data.frame(Observed_val         = obs_values["Nestedness", "Val_Obs"],
                                 Mean_null_val        = mean(nestedness),
                                 Percentiles_2.5_97.5 = paste0("[",round(quantile(nestedness, c(0.025, 0.975))[1], 2), " ; ", round(quantile(nestedness, c(0.025, 0.975))[2], 2), "]"))
          rownames(nest_res) <- "Nestedness"
          
        } else {
          cat("Shapiro test, Nestedness p.value:", round(shapiro_nest, digits = 5),"\n The 999 null values follow a normal distribution\n", ">>> You can calculate Standardize Effect Size")
          
          # Store results into a data frame and compute SES and pvals ==> 2*pnorm for bilateral test
          nest_res <- NCSSDGproj::SES_pval(val_obs = obs_values["Nestedness", "Val_Obs"], mean_null = mean(nestedness), sd_null = sd(nestedness), rowname = "Nestedness")
        }
    
      # Produce and save normality plots
      hist_mod <- ggplot2::ggplot(as.data.frame(modularity_vals), ggplot2::aes(x = modularity_vals)) + ggplot2::xlab("null values") + ggplot2::geom_histogram(color = "black", fill = "grey80") + ggplot2::annotate(geom = "text", x = -Inf, y = Inf, vjust = 1.2, hjust = -0.02, size = 3, label = paste("Shapiro test, p.value = ", signif(shapiro_mod, 3))) + ggplot2::ggtitle("Modularity") + ggplot2::theme_bw() +  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) 
      hist_nest <- ggplot2::ggplot(as.data.frame(nestedness), ggplot2::aes(x = nestedness)) + ggplot2::xlab("null values") + ggplot2::geom_histogram(color = "black", fill = "grey80") + ggplot2::annotate(geom = "text", x = -Inf, y = Inf, vjust = 1.2, hjust = -0.02, size = 3, label = paste("Shapiro test, p.value = ", signif(shapiro_nest, 3))) +  ggplot2::ggtitle("Nestedness") + ggplot2::theme_bw() +  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) 
      qqplot_mod <- ggpubr::ggqqplot(modularity_vals) + ggplot2::theme_bw() + ggplot2::annotate(geom = "text", x = -Inf, y = Inf, vjust = 1.2, hjust = -0.02, size = 3, label = paste("Shapiro test, p.value = ", signif(shapiro_mod, 3)))
      qqplot_nest <- ggpubr::ggqqplot(nestedness) + ggplot2::theme_bw() + ggplot2::annotate(geom = "text", x = -Inf, y = Inf, vjust = 1.2, hjust = -0.02, size = 3, label = paste("Shapiro test, p.value = ", signif(shapiro_nest, 3)))
      qqplots <- (hist_mod | hist_nest)/(qqplot_mod | qqplot_nest)
      ggplot2::ggsave(here::here("figures", "Supp_fig7_sensi.png"), qqplots, width = 9, height = 5, device = "png")
      save(qqplots, file = here::here("results", "Supp_fig7_sensi.RData"))
      
    
    ## For TUI and TOI
    shapiro_TUI <- shapiro.test(insurance_vals$TUI)$p.value
    shapiro_TOI <- shapiro.test(insurance_vals$TOI)$p.value
    
      # Choice between SES computation and 2.5 and 07.5 percentiles
    
        # For TUI
        if(shapiro_TUI < 0.05){
          cat("Shapiro test, TUI p.value:", signif(shapiro_TUI, digits = 3),
              "\n The 999 null values does not follow a normal distribution\n", 
              ">>> You can't calculate Standardize Effect Size \n \n",
              "2.5th and 97.5 Percentiles nestedness: \n",
              quantile(insurance_vals$TUI, c(0.025, 0.975)),
              "\n TUI observed:", obs_values["TUI", "Val_Obs"])
          
          TUI_res <- data.frame(Observed_val         = obs_values["TUI", "Val_Obs"],
                                Mean_null_val        = mean(insurance_vals$TUI),
                                Percentiles_2.5_97.5 = paste0("[",round(quantile(insurance_vals$TUI, c(0.025, 0.975))[1], 3), " ; ", round(quantile(insurance_vals$TUI, c(0.025, 0.975))[2], 3), "]"))
          rownames(TUI_res) <- "TUI"
          
        } else {
          cat("Shapiro test, TUI p.value:", round(shapiro_TUI, digits = 5),"\n The 999 null values follow a normal distribution\n", ">>> You can calculate Standardize Effect Size")
          
          # Store results into a data frame and compute SES and pvals ==> 2*pnorm for bilateral test
          TUI_res <- NCSSDGproj::SES_pval(val_obs = obs_values["TUI", "Val_Obs"], mean_null = mean(insurance_vals$TUI), sd_null = sd(insurance_vals$TUI), rowname = "TUI") 
        }
    
    
        # For TOI
        if(shapiro_TOI < 0.05){
          cat("Shapiro test, TOI p.value:", signif(shapiro_TOI, digits = 3),
              "\n The 999 null values does not follow a normal distribution\n", 
              ">>> You can't calculate Standardize Effect Size \n \n",
              "2.5th and 97.5 Percentiles nestedness: \n",
              quantile(insurance_vals$TOI, c(0.025, 0.975)),
              "\n TOI observed:", obs_values["TOI", "Val_Obs"])
          
          TOI_res <- data.frame(Observed_val         = obs_values["TOI", "Val_Obs"],
                                Mean_null_val        = mean(insurance_vals$TOI),
                                Percentiles_2.5_97.5 = paste0("[",round(quantile(insurance_vals$TOI, c(0.025, 0.975))[1], 3), " ; ", round(quantile(insurance_vals$TOI, c(0.025, 0.975))[2], 3), "]"))
          rownames(TOI_res) <- "TOI"
          
        } else {
          cat("Shapiro test, TOI p.value:", round(shapiro_TOI, digits = 3),"\n The 999 null values follow a normal distribution\n", ">>> You can calculate Standardize Effect Size")
          
          # Store results into a data frame and compute SES and pvals ==> 2*pnorm for bilateral test
          TOI_res <- NCSSDGproj::SES_pval(val_obs = obs_values["TOI", "Val_Obs"], mean_null = mean(insurance_vals$TOI), sd_null = sd(insurance_vals$TOI), rowname = "TUI") 
        }  
    
      
      # Produce and save normality plots
      hist_TUI <- ggplot2::ggplot(as.data.frame(insurance_vals), ggplot2::aes(x = TUI)) + ggplot2::xlab("null values") + ggplot2::geom_histogram(color = "black", fill = "grey80") + ggplot2::ggtitle("TUI") + ggplot2::theme_bw() +  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + ggplot2::annotate(geom = "text", x = -Inf, y = Inf, vjust = 1.2, hjust = -0.02, size = 3, label = paste("Shapiro test, p.value = ", signif(shapiro_TUI, 3)))
      hist_TOI <- ggplot2::ggplot(as.data.frame(insurance_vals), ggplot2::aes(x = TOI)) + ggplot2::xlab("null values") + ggplot2::geom_histogram(color = "black", fill = "grey80") + ggplot2::ggtitle("TOI") + ggplot2::theme_bw() +  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + ggplot2::annotate(geom = "text", x = -Inf, y = Inf, vjust = 1.2, hjust = -0.02, size = 3, label = paste("Shapiro test, p.value = ", signif(shapiro_TOI, 3)))
      qqplot_TUI <- ggpubr::ggqqplot(insurance_vals$TUI) + ggplot2::theme_bw() + ggplot2::annotate(geom = "text", x = -Inf, y = Inf, vjust = 1.2, hjust = -0.02, size = 3, label = paste("Shapiro test, p.value = ", signif(shapiro_TUI, 3)))
      qqplot_TOI <- ggpubr::ggqqplot(insurance_vals$TOI) + ggplot2::theme_bw() + ggplot2::annotate(geom = "text", x = -Inf, y = Inf, vjust = 1.2, hjust = -0.02, size = 3, label = paste("Shapiro test, p.value = ", signif(shapiro_TOI, 3)))
      qqplots <- (hist_TUI | hist_TOI)/(qqplot_TUI | qqplot_TOI)
      ggplot2::ggsave(here::here("figures", "Supp_fig8_sensi.png"), qqplots, width = 9, height = 5, device = "png")
      save(qqplots, file = here::here("results", "Supp_fig8_sensi.RData"))
    
    ## Bind data
    sensit_ana_res <- list("modularity" = mod_res, "nestedness" = nest_res, "TUI" = TUI_res, "TOI" = TOI_res)
    
  ### Saving output
  if(save == TRUE) {
    
    save(sensit_ana_res, file = here::here("results", paste0(name, ".RData")))
    
  }
    
  return(sensit_ana_res)
  
}



