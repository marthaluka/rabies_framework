# libraries

require(pacman)
pacman::p_load(tidyverse, # cleaning, wrangling
               sf, # spatial manipulation
               RColorBrewer, # color palettes
               scales, # format numbers for aesthetics
               patchwork,  # multiple plots
               data.tableb# data tables for memory efficiency
)

# source model
source("./scripts/stochastic_decision_tree.R")

# read shapefile #####
east_africa_shp <- st_read(dsn="./shapefiles/", layer="ea_shapefile")
east_africa_shp <- east_africa_shp[-c(135, 117, 153), ]
east_africa_shp$Population <- as.numeric(east_africa_shp$Population) 
east_africa_shp$county_id <- rownames(east_africa_shp)


# Run the model across districts

#subset to set up code
districts <- data.table(
  district_id = east_africa_shp$county_id,
  pop = east_africa_shp$Population
)

## read parameters file
parameters_df <- read.csv("./data/parameters.csv")

# extract parameter values from csv
run_decision_tree_from_csv <- function(scenario_name, parameters_df, pop=50000000, horizon=10, base_vax_cov=0.05, N=100){
  scenario_parameters <- parameters_df[parameters_df$scenario == scenario_name, ]
  
  decision_tree(
    N = N,
    pop = pop,
    horizon = horizon, 
    base_vax_cov=base_vax_cov,
    discount = scenario_parameters$discount,
    target_vax_cov = scenario_parameters$target_vax_cov,
    # epidemiological status quo
    #LR_range = c(scenario_parameters$LR_range1, scenario_parameters$LR_range1),
    HDR = c(scenario_parameters$HDR1, scenario_parameters$HDR2),
    pBite_healthy = scenario_parameters$pBite_healthy,
    
    mu = scenario_parameters$mu,
    k = scenario_parameters$k,
    # health seeking - healthy bites
    pSeek_healthy = scenario_parameters$pSeek_healthy,
    pStart_healthy = scenario_parameters$pStart_healthy,
    pComplete_healthy = scenario_parameters$pComplete_healthy,
    # health seeking - rabid bites
    pSeek_exposure = scenario_parameters$pSeek_exposure,
    pStart_exposure = scenario_parameters$pStart_exposure,
    pComplete_exposure = scenario_parameters$pComplete_exposure,
    # biological params
    pDeath = scenario_parameters$pDeath,
    pPrevent = scenario_parameters$pPrevent,
    # economics
    full_cost = scenario_parameters$full_cost,
    partial_cost = scenario_parameters$partial_cost,
    vaccinate_dog_cost = c(scenario_parameters$vaccinate_dog_cost1, scenario_parameters$vaccinate_dog_cost2),
    # campaign cost
    #IBCM
    pInvestigate = scenario_parameters$pInvestigate,
    pFound = scenario_parameters$pFound,
    pTestable = scenario_parameters$pTestable,
    pFN = scenario_parameters$pFalseNeg
  )
}


# Nested function to summarise a given variable from a given scenario
summarise_variables_per_scenario <- function(scenario_list) {       # a list for a given scenario eg no_interventions
  
  summarise_variable <- function(variable, scenario) {
    my_matrix <- scenario[[variable]]
    
    summarise_stochasticity <- function(my_matrix) {
      out <- apply(my_matrix, 2, quantile, c(0.025, 0.5, 0.975), na.rm=TRUE)
      rownames(out) <- NULL
      return(out)
    }
    
    out <- summarise_stochasticity(my_matrix)
    df <- as.data.frame(t(out))
    names(df) <- c('LL', 'Median', 'UL')
    df$year <- rownames(df)
    df$variable <- variable  # Capture the variable name
    
    return(df)
  }
  
  # Summarise variables across districts/counties
  all_summaries <- lapply(1:length(scenario_list), function(idx) {
    scenario <- scenario_list[[idx]]
    # Loop through all variables in the current scenario
    scenario_summaries <- lapply(names(scenario), function(var) {
      df <- summarise_variable(variable = var, scenario = scenario)
      df$county_id <- idx
      return(df)
    })
    # Combine all results for the current scenario
    do.call(rbind, scenario_summaries)
  })
  
  combined_results <- do.call(rbind, all_summaries)
  
  return(combined_results)
}


# Run model across districts and summarise stochasticicty
    ## Prediction tables
no_interventions <- data.table(
  summarise_variables_per_scenario(
    lapply(districts$pop, function(pop_value) {
      run_decision_tree_from_csv("no_interventions", parameters_df, pop_value)
    })
  )
)


PEP_IM_free_only <- data.table(
  summarise_variables_per_scenario(
    lapply(districts$pop, function(pop_value) {
      run_decision_tree_from_csv("PEP_IM_free_only", parameters_df, pop_value)
    })
  )
)



PEP_ID_free_only <- data.table(
  summarise_variables_per_scenario(
    lapply(districts$pop, function(pop_value) {
      run_decision_tree_from_csv("PEP_ID_free_only", parameters_df, pop_value)
    })
  )
)


MDV_only <- data.table(
  summarise_variables_per_scenario(
    lapply(districts$pop, function(pop_value) {
      run_decision_tree_from_csv("MDV_only", parameters_df, pop_value)
    })
  )
)


MDV_PEP_IM_free <- data.table(
  summarise_variables_per_scenario(
    lapply(districts$pop, function(pop_value) {
      run_decision_tree_from_csv("MDV_PEP_IM_free", parameters_df, pop_value)
    })
  )
)

MDV_PEP_ID_free <- data.table(
  summarise_variables_per_scenario(
    lapply(districts$pop, function(pop_value) {
      run_decision_tree_from_csv("MDV_PEP_ID_free", parameters_df, pop_value)
    })
  )
)


#  Other Tables 
## District/ Geography table
east_africa_shp


## Scenarios table
parameters_df$scenario_id <- seq(1, length(parameters_df$scenario))
parameters_df


## Variables table
variable_df <- data.table(
  variable = unique(no_interventions$variable),
  variable_id = seq(1, length(unique(no_interventions$variable)))
)


# Combine all predictions
    # Add a scenario column to each data table, # Make sure this matches scenario_id in parameters_df
no_interventions[, scenario := 1]
PEP_IM_free_only[, scenario := 2]
PEP_ID_free_only[, scenario := 3]
MDV_only[, scenario := 4]
MDV_PEP_IM_free[, scenario := 5]
MDV_PEP_ID_free[, scenario := 6]

# Combine them using rbindlist
combined_data <- rbindlist(list(no_interventions, PEP_IM_free_only, PEP_ID_free_only, MDV_only, MDV_PEP_IM_free, MDV_PEP_ID_free))

# Export to CSV vs write a db (memory check)
fwrite(combined_data, "./data/combined_predictions.csv")

object.size(combined_data)


# Database








