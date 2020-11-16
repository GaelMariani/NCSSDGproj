
#' Target Under Insurance And Over Insurance
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
  
  
  # TUI, percentage of target achieved by only one ecosystem
  for(nrow in 1:nrow(data_TI)){
    data_TI$min[nrow] <- min(data_TI$value[nrow]-1, 1)
  }
  
  target_under_insurance <- (Ntarget - sum(data_TI$min))/Ntarget
  
  # TOI, percentage of NCS in excess in target having more NCS than expected from target redundancy
  Target_insurance <- sum(data_TI$value)/Ntarget  # Target Insurance the mean number of ecosystem that help in achieving a target
  
  for(nrow in 1:nrow(data_TI)){
    data_TI$max[nrow] <- max(data_TI$value[nrow], Target_insurance)
  }
  
  target_over_insurance <- (sum(data_TI$max - Target_insurance))/sum(data_TI$value)
  
   
  TUI_TOI_TI <- data.frame(TUI = target_under_insurance,
                           TI = Target_insurance,
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
  
  results_nm <- data.frame(Val_Obs = val_obs,
                           Val_nulls =  mean_null,
                           SES = (val_obs - mean_null)/sd_null,
                           pvalue = 2*pnorm(-abs((val_obs - mean_null)/sd_null)))
  
  rownames(results_nm) <- rowname
  
  return(results_nm)
            
}


#' Null Model Analysis
#'
#' @param matrix01 formatted matrix to calculate network indices, nestedness and modularity. Use data_netw_indice to format
#' @param NMalgo name of null model algorithm => r00 for Insurance and quasiswap for modularity and nestedness
#' @param NESTmethod "NODF" or "weighted NODF"
#' @param TargetInsurance if TRUE, compute Insurance indices, if FALSE calculate  network indices
#' @param rawdata a dataframe with targets of the SDGs in columns and NCSs in rows
#' @param Nrun 	Number of replicate runs for metaComputesModules 
#' @param Nsim Number of null matrices
#'
#' @return a 3 column dataframe with null model results for nestedness and modularity OR TOi and TUI
#' @export
#'
#' @examples
NullModels <- function(matrix01, rawdata, NMalgo, NESTmethod, Nrun, Nsim, TargetInsurance = FALSE) {
  
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
    
    # Store results into a data frame and compute SES and pvals ==> 2*pnorm for bilateral test
    mod_res <- NCSSDGproj::SES_pval(val_obs = modularity_obs@likelihood, mean_null = mean(mod_null), sd_null = sd(mod_null), rowname = "Modularity")
    nest_res <- NCSSDGproj::SES_pval(val_obs = nestedness_obs, mean_null = mean(nest_null), sd_null = sd(nest_null), rowname = "Nestedness")
     
    # Bind data
    mod_nest_res <- rbind(mod_res, nest_res)
    
  return(mod_nest_res)
    
  }
  
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
      indices_obs <- NCSSDGproj::TUI_TOI(TI_data_obs, Necosystem = 11, Ntarget = 84)
    
    ## Null matrices 
    nm_r00 <- stats::simulate(vegan::nullmodel(matrix01, NMalgo), nsim = Nsim) # Returns a list of matrices
    nm_r00 <- asplit(nm_r00, 3)
    
      # Format data to calculate indices
      data_TI <- lapply(nm_r00, NCSSDGproj::data_TI)
      
      # Calculate TOI and TUI on null matrices
      indices_NM <- lapply(data_TI, NCSSDGproj::TUI_TOI, Necosystem = 11, Ntarget = 84)
      indices_nullmod_df <- do.call(rbind, indices_NM)
      
    ## Statistic tests
      
      # Store results into a data frame and compute SES and pvals ==> 2*pnorm for bilateral test
      TUI_res <- NCSSDGproj::SES_pval(val_obs = indices_obs$TUI, mean_null = mean(indices_nullmod_df$TUI), sd_null = sd(indices_nullmod_df$TUI), rowname = "TUI") 
      TOI_res <- NCSSDGproj::SES_pval(val_obs = indices_obs$TOI, mean_null = mean(indices_nullmod_df$TOI), sd_null = sd(indices_nullmod_df$TOI), rowname = "TOI") 
      
      # Bind results
      TUI_TOI_res <- rbind(TUI_res, TOI_res)
      
    return(TUI_TOI_res)
      
  } # end ELSE
  
}