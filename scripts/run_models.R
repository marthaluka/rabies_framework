

# libraries

require(pacman)
pacman::p_load(tidyverse, # cleaning, wrangling
               scales,    # display neat number values
               paletteer  # cool color palettes
               )

# source model
source("./scripts/stochastic_decision_tree.R")


# scenarios

## read parameters file
parameters_df <- read.csv("./data/parameters.csv")

# extract parameter values from csv
run_decision_tree_from_csv <- function(scenario_name, parameters_df, pop=50000000, horizon = 7,base_vax_cov=0.05, N = 100){
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

# run this across all variables and store the summarised object instead




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
  compare_df <- as.data.frame(do.call("rbind", compare_df_list))
  
  # Convert list columns into a more manageable data type
  for (col_name in names(compare_df)) {
    if (is.list(compare_df[[col_name]])) {
      compare_df[[col_name]] <- as.character(unlist(compare_df[[col_name]]))
    }
  }
  
  return(compare_df)
  
}

my_data<- compare_scenarios("ts_MDV_campaign_cost", no_interventions, MDV_only, MDV_PEP_ID_free)


# plot

# plot_slope_chart <- function(data) {
#   # Ensure data is ordered by scenario
#   #data <- data[order(data$scenario), ]
#   
#   # Define the color palette
#   colors <- paletteer_d("beyonce::X6")
#   
#   ggplot(my_data, aes(x = scenario, y = Median, group = 1)) +
#     geom_line(color = "grey", size = 0.5) +
#     geom_point(aes(y = Median, color = scenario), size = 3) #+
#     # #geom_crossbar(aes(ymin = LL, ymax = UL, color = scenario), width = 0.2, size = 0.5) +
#     # geom_linerange(aes(ymin = LL, ymax = UL, color = scenario), size = 0.5) +
#     # labs(y = "Value", x = "Scenario") +
#     # #theme_minimal() +
#     # #scale_color_manual(values = colors, name = "Scenario") +
#     # scale_y_continuous(labels = comma)+ 
#     # theme(legend.position = "none")
# }
# 
# plot_slope_chart(my_data)
# 
# 
# as.factor(unlist(my_data$scenario))
# 
# 
# data <- my_data$LL



