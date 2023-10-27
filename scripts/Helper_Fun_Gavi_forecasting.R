


# Run model to extract variable

# source model
source("./scripts/stochastic_decision_tree.R")

## read parameters file
parameters_df <- read.csv("./data/parameters.csv")

# run model, selecting only variables of interest
run_decision_tree_and_select_variables2 <- function(scenario_name, parameters_df, pop=350000, horizon = 2, base_vax_cov=0.05, 
                                                    N = 1000, HDR, selected_vars=NULL){
  scenario_parameters <- parameters_df[parameters_df$scenario == scenario_name, ]
  
  result <- decision_tree(
    N = N,
    pop = pop,
    horizon = horizon, 
    base_vax_cov=base_vax_cov,
    discount = scenario_parameters$discount,
    target_vax_cov = scenario_parameters$target_vax_cov,
    HDR = HDR,
    pBite_healthy = scenario_parameters$pBite_healthy,
    mu = scenario_parameters$mu,
    k = scenario_parameters$k,
    pSeek_healthy = scenario_parameters$pSeek_healthy,
    pStart_healthy = scenario_parameters$pStart_healthy,
    pComplete_healthy = scenario_parameters$pComplete_healthy,
    pSeek_exposure = scenario_parameters$pSeek_exposure,
    pStart_exposure = scenario_parameters$pStart_exposure,
    pComplete_exposure = scenario_parameters$pComplete_exposure,
    pDeath = scenario_parameters$pDeath,
    pPrevent = scenario_parameters$pPrevent,
    full_cost = scenario_parameters$full_cost,
    partial_cost = scenario_parameters$partial_cost,
    vaccinate_dog_cost = c(scenario_parameters$vaccinate_dog_cost1, scenario_parameters$vaccinate_dog_cost2),
    pInvestigate = scenario_parameters$pInvestigate,
    pFound = scenario_parameters$pFound,
    pTestable = scenario_parameters$pTestable,
    pFN = scenario_parameters$pFalseNeg
  )
  
  # If specific variables are provided, subset the output to only those variables
  if(!is.null(selected_vars) && is.vector(selected_vars)){
    result <- result[selected_vars]
  }
  
  return(result)
}


# find upper limit, median and lower limit from list of length N
summarise_stochasticity <-  function(my_matrix){
  out<- apply(my_matrix, 2, quantile, c(0.025, 0.5, 0.975), na.rm=TRUE)
  rownames(out)<-NULL
  return(out)
}


# extract monthly value from annual predictions
extract_monthly <- function(matrix_list, HDR_list) {
  # Extract only the first year
  first_column_list <- lapply(matrix_list, function(x) {
    x[, 1]
  })
  
  # Get monthly value from annual prediction
  result_df <- as.data.frame(do.call(rbind, first_column_list) / 12)
  names(result_df) <- c('LL', 'Median', 'UL')
  
  # Label with respective HDR
  HDR_string <- sapply(HDR_list, function(x) paste(x, collapse = "-"))
  result_df$HDR <- HDR_string
  
  return(result_df)
}





