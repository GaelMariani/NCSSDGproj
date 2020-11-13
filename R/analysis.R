
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
  
  
  # TUI, the mean number of ecosystem that help in achieving a target
  for(nrow in 1:nrow(data_TI)){
    data_TI$min[nrow] <- min(data_TI$value[nrow]-1, 1)
  }
  
  target_under_insurance <- (Ntarget - sum(data_TI$min))/Ntarget
  
  # TOI, percentage of NCS in excess in target having more NCS than expected from target redundancy
  Target_insurance <- sum(data_TI$value)/Ntarget  # Target Insurance the mean number of ecosystem that help in achieving a target
  
  for(nrow in 1:nrow(data_TI)){
    data_TI$max[nrow] <- max(data_TI$value[nrow], Target_redundancy)
  }
  
  target_over_insurance <- (sum(data_TI$max - Target_redundancy))/sum(data_TI$value)
  
   
  TUI_TOI_TI <- data.frame(TUI = target_under_insurance,
                           TI = Target_insurance,
                           TOI = target_over_insurance)
  
  
  return(TUI_TOI_TI)
}



#' Null Model Analysis
#'
#' @param matrix01 formatted matrix to calculate network indices, nestedness and modularity
#' @param NMalgo name of null model algorithm
#' @param NESTmethod "NODF" or "weighted NODF"
#' @param TargetInsurance if TRUE, compute Insurance indices, not network indices
#' @param rawdata a dataframe with targets of the SDGs in columns and NCSs in rows
#' @param Nrun 	Number of replicate runs
#'
#' @return
#' @export
#'
#' @examples
NullModels <- function(matrix01, rawdata, NMalgo, NESTmethod = c("NODF", "weighted NODF"), Nrun, TargetInsurance = FALSE) {
  
  ## Compute NESTEDNESS and MODULARITY
  if(TargetInsurance == FALSE){
    
    if(NESTmethod == "NODF") {
      nestedness <- bipartite::nested(matrix01, "NODF")
      modularity <- bipartite::metaComputeModules(matrix01, N = Nrun)
    } 
    
    if(NESTmethod == "weighted NODF") {
      nestedness <- bipartite::nested(matrix01, "weighted NODF")
      modularity <- bipartite::metaComputeModules(matrix01, N = Nrun)
    }
    
    else stop("Method not available")
    
    
    
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
      indices_obs <- NCSSDGproj::TUI_TOI(TI_data_obs, Necosystem = 11, Ntarget = nrow(data_TI))
    
    ## Null matrices 
    nm_r00 <- stats::simulate(vegan::nullmodel(matrix01, "r00"), nsim=999) # Returns a list of matrices
    nm_r00 <- asplit(nm_r00, 3)
    
      # Format data to calculate indices
      data_TI <- lapply(nm_r00, NCSSDGproj::data_TI)
      
      # Calculate TOI and TUI on null matrices
      indices_NM <- lapply(data_TI, NCSSDGproj::TUI_TOI, Necosystem = 11, Ntarget = nrow(data_TI))
      indices_nullmod_df <- do.call(rbind, indices_NM)
      
    ## Statistic tests
      
      
  } # end ELSE
  
}