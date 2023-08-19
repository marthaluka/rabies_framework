

# libraries

require(pacman)
pacman::p_load(tidyverse, # cleaning, wrangling
               sf, # spatial manipulation
               leaflet, # leaflet maps
               shiny, # interactive web apps
               shinycssloaders, # loading symbol for app
               RColorBrewer, # color palettes
               htmltools, # HTML generation and tools
               scales, # format numbers for aesthetics
               patchwork, # multiple plots
               DT # data objects (matrices or data frames)
               )

# source model
source("./scripts/stochastic_decision_tree.R")

# read shapefile #####
east_africa_shp <- st_read(dsn="./shapefiles/", layer="ea_shapefile")
east_africa_shp$Population <- as.numeric(east_africa_shp$Population) 


# countries in shapefile
countries <- sort(unique(east_africa_shp$Country))

# for app --to review this
# variables for selection. Can add or reduce
variables <- c("Population", "dog_population", "rabid_dogs", "total_rabid_bites", 
               "total_people_PEP", "rabies_deaths", "lives_saved", "cost_per_life_saved")


# scenarios

## read parameters file
parameters_df <- read.csv("./data/parameters.csv")

# extract parameter values from csv
run_decision_tree_from_csv <- function(scenario_name, parameters_df){
  scenario_parameters <- parameters_df[parameters_df$scenario == scenario_name, ]
  
  decision_tree(
    N = 100,
    pop = 50000000,
    horizon = 7, 
    base_vax_cov=0.05,
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

# scenarios on a given population 
no_interventions <- run_decision_tree_from_csv("no_interventions", parameters_df)
PEP_IM_free_only <- run_decision_tree_from_csv("PEP_IM_free_only", parameters_df)
PEP_ID_free_only <- run_decision_tree_from_csv("PEP_ID_free_only", parameters_df)
MDV_only <- run_decision_tree_from_csv("MDV_only", parameters_df)
MDV_PEP_ID_free <- run_decision_tree_from_csv("MDV_PEP_ID_free", parameters_df)



# find upper limit, median and lower limit from list of length N
summarise_stochasticity <-  function(my_matrix){
  out<- apply(my_matrix, 2, quantile, c(0.025, 0.5, 0.975), na.rm=TRUE)
  rownames(out)<-NULL
  return(out)
}

# select variable from an object/scenario
select_variable <- function(variable, scenario){
  my_matrix <- scenario[[variable]]
  out<- summarise_stochasticity(my_matrix)
  df <- as.data.frame(t(out))
  names(df) <- c('LL', 'Median', 'UL')
  return(df)
}

names(no_interventions)

# return time series values 
df <- select_variable(variable='ts_rabid_dogs', scenario=no_interventions)


# Plotting to check
ggplot(df, aes(x = as.numeric(row.names(df)), y = Median)) +
  geom_line() +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
  ylab("Value")+ xlab("Year")+
  theme_bw() 


# Compare >2 scenarios (using cumulative values over horizon ie instead of yearly)
compare_scenarios <- function(variable, ...){
  # Get the list of scenarios from arguments
  scenarios <- list(...)
  scenario_names <- as.character(substitute(list(...)))[-1]  # Capture the names of the scenarios
  
  # Process each scenario
  compare_df_list <- mapply(function(scenario, name) {
    df <- colSums(select_variable(variable, scenario))
    df$scenario <- name  # Add the scenario name as a new column
    return(df)
  }, scenario = scenarios, name = scenario_names, SIMPLIFY = FALSE)
  
  # Combine the results into one dataframe
  compare_df <- do.call("rbind", compare_df_list)
  
  return(as.data.frame(compare_df))
}

compare_scenarios("ts_MDV_campaign_cost", no_interventions, MDV_only, MDV_PEP_ID_free)


# plot to check



MDV_only$ts_MDV_campaign_cost

no_interventions$ts_MDV_campaign_cost







