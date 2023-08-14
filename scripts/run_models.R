

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
               patchwork # multiple plots
               )

# source model
source("./scripts/stochastic_decision_tree.R")

# read shapefile #####
east_africa_shp <- st_read(dsn="./shapefiles/", layer="ea_shapefile")
east_africa_shp$Population <- as.numeric(east_africa_shp$Population) 

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
    discount = scenario_parameters$discount,
    target_vax_cov = scenario_parameters$target_vax_cov,
    # epidemiological status quo
    rabies_inc = c(scenario_parameters$rabies_inc1, scenario_parameters$rabies_inc2),
    LR_range = c(scenario_parameters$LR_range1, scenario_parameters$LR_range1),
    HDR = c(scenario_parameters$HDR1, scenario_parameters$HDR2),
    
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
df <- select_variable(variable='ts_MDV_campaign_cost', scenario=PEP_ID_free_only)


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








######################

# iterate through different possible vaccination coverages. 
  # We select a step of 0.1 
vax_covs <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)



# Deterministic function #####
source("./scripts/deterministic_function.R")

## Policy option 1: PEP under status quo i.e., patient pays #####
  ### loop through different vax coverage for policy option 1

  # empty df to append results
output_det_model1 = data.frame()

  # loop
for (vax_cov in vax_covs){
  model_output<- deterministic_decision_tree(pop = east_africa_shp$Population, 
                                             HDR=20,              
                                             vax_cov=vax_cov,
                                             incidence=0.01,         # incidence with no interventions in place
                                             P_bite_rabid=0.38,      # p=0.375
                                             P_seek_care_rabid_bite = 0.75,
                                             P_initiate_PEP_rabid_bite = 0.79,       # of those who seek care (not rabid bites)
                                             P_complete_PEP_rabid_bite = 0.473,      # of those who seek healthcare
                                                # Also using same values for healthy bites despite PEP policy choice
                                             P_seek_care_healthy_bite = 0.78,
                                             P_initiate_PEP_healthy_bite = 0.2, 
                                             P_complete_PEP_healthy_bite = 0.07,
                                             P_death=0.17,                  # 0.133-0.201 -  Changalucha et al 2019
                                             P_prevent=0.99
                                             ) 
  
  model_output$vax_cov <- vax_cov
  model_output <- cbind(east_africa_shp, model_output)
  output_det_model1 <- rbind(output_det_model1, model_output)
}

  # state policy choice
output_det_model1$policy_choice <- "Offered under status quo"

####

## Policy option 2: free PEP. The health seeking patterns/ probabilities change #####
  ### loop through different vax coverages for policy option 2

  # empty df to append results
output_det_model2 = data.frame()

  # loop
for (vax_cov in vax_covs){
  model_output<- deterministic_decision_tree(pop = east_africa_shp$Population, 
                                             HDR=20,                  
                                             vax_cov=vax_cov,
                                             incidence=0.01,          # incidence with no interventions in place
                                             P_bite_rabid=0.38,       # p=0.375
                                             P_seek_care_rabid_bite = 0.75,           # not clear from Changalucha et al., (using value for IF patient pays/ status quo)
                                             P_initiate_PEP_rabid_bite = 0.899, 
                                             P_complete_PEP_rabid_bite = 0.542, 
                                                # Also using same values for healthy bites despite PEP policy choice
                                             P_seek_care_healthy_bite = 0.78,
                                             P_initiate_PEP_healthy_bite = 0.2, 
                                             P_complete_PEP_healthy_bite = 0.07,
                                             P_death=0.17,             # 0.133-0.201 -  Changalucha et al 2019
                                             P_prevent=0.99
                                             ) 
  
  model_output$vax_cov <- vax_cov
  model_output <- cbind(east_africa_shp, model_output)
  output_det_model2 <- rbind(output_det_model2, model_output)
}

  # state policy choice
output_det_model2$policy_choice <- "Offered free of charge"

# merge into one df 
output_det_model <- rbind(output_det_model1, output_det_model2)





# Stochastic function #####
source("./scripts/stochastic_function.R")


#  A function to create a Vector with columns (summarise stochasticity) ## mean, LL- lower limit and UL- upper limit
create_new_names <- function(col_name){
  name1 <- paste0(col_name, "_mean")
  name2 <- paste0(col_name, "_LL")
  name3 <- paste0(col_name, "_UL")
  return(list(name1, name2, name3))
} 


#  A function to summarize model output (into mean, upper and lower limits) and store in data frame
summarise_stochastic_model_output <- function(model_output){
  #Create empty data frame with correct number of columns
    stochastic_model_df = data.frame(matrix(nrow = 0, ncol = length(names(model_output[[1]]))*3)) # each variable/col now resolves into 3: mean, upper and lower limits
    colnames(stochastic_model_df) = unlist(lapply(names(model_output[[1]]), create_new_names), recursive = FALSE) # get new variable names using `create_new_names` function
  
    # Summarize model output(mean, upper and lower limits)  
  for (dataf in seq_along(model_output)) {
    my_list = list()
    for (variable in seq(1,ncol(model_output[[1]]))){
      projections <- model_output[[dataf]][[variable]]
      mean_projections <- mean(projections)
      sd_projections <- sd(projections)
      margin <- qt(0.975,df=length(projections)-1)*sd_projections/sqrt(length(projections))
      lowerinterval <- mean_projections - margin
      upperinterval <- mean_projections + margin
      # We end up with 3 new values from every column (mean, upper limit and lower limit)
      output <- list(mean_projections, lowerinterval, upperinterval)
      my_list <- append(my_list, output)
    }
    # create new row to merge projections per district
    df1 <- data.frame(my_list)
    colnames(df1) = unlist(lapply(names(model_output[[1]]), create_new_names), recursive = FALSE)
    stochastic_model_df <- rbind(stochastic_model_df, df1)
  }
  stochastic_model_df <- cbind(east_africa_shp, stochastic_model_df)
  return(stochastic_model_df)
}

## Policy option 1: PEP under status quo i.e., patient pays #####
    ### loop through different vax coverage for policy option 1

# empty df to append results
output_stoch_model1 = data.frame()

# Run model
loop_thru_vaxs1<- function(){
  for (vax_cov in vax_covs){
    model_output <- lapply(east_africa_shp$Population, stochastic_decision_tree, 
                           N=1000,
                           HDR_min=4.5, HDR_max=235.3,  
                           vax_cov=vax_cov,
                           inc_min=0.005, inc_max=0.015, 
                           P_bite_rabid=0.38, 
                           P_seek_care_rabid_bite=0.75,
                           P_initiate_PEP_rabid_bite=0.79, 
                           P_complete_PEP_rabid_bite=0.473,
                           P_seek_care_healthy_bite=0.78,
                           P_initiate_PEP_healthy_bite=0.2, 
                           P_complete_PEP_healthy_bite=0.07,
                           P_death_min=0.133, P_death_max = 0.201,  
                           P_prevent_min=0.97, P_prevent_max=1)
    
    # summarise stochasticity
    stochastic_model_df <- summarise_stochastic_model_output(model_output)
    # note the vaccination coverage
    stochastic_model_df$vax_cov <- vax_cov 
    # Bind output (of every vax_cov) to single data frame
    output_stoch_model1 <- rbind(output_stoch_model1, stochastic_model_df)
  }
  return(output_stoch_model1)
}

output_stoch_model1<- loop_thru_vaxs1()


  # note the policy choice
output_stoch_model1$policy_choice <- "Offered under status quo" 


####
## Policy option 2: free PEP. The health seeking patterns/ probabilities change #####
  ### loop through different vax coverages for policy option 2

# empty df to append results
stochastic_model_df2 = data.frame()

# loop
loop_thru_vaxs2<- function(){
  for (vax_cov in vax_covs){
    model_output2<- lapply(east_africa_shp$Population, stochastic_decision_tree, 
                           N=1000,                           # 1000 iterations
                           HDR_min=4.5, HDR_max=235.3, 
                           vax_cov=vax_cov,
                           inc_min=0.005, inc_max=0.015, 
                           P_bite_rabid=0.38, 
                           P_seek_care_rabid_bite=0.75,     # not clear from Changalucha et al., (using value for IF patient pays/ status quo)
                           P_initiate_PEP_rabid_bite=0.899, 
                           P_complete_PEP_rabid_bite=0.542,
                           # No data here there4 using same values for healthy bites despite PEP policy choice
                           P_seek_care_healthy_bite=0.78,
                           P_initiate_PEP_healthy_bite=0.2, 
                           P_complete_PEP_healthy_bite=0.07,
                           P_death_min=0.133, P_death_max = 0.201,  
                           P_prevent_min=0.97, P_prevent_max=1
                           )
    
    model_output_summarised <- summarise_stochastic_model_output(model_output2)
    model_output_summarised$vax_cov <- vax_cov
    stochastic_model_df2 <- rbind(stochastic_model_df2, model_output_summarised)
  }
  return(stochastic_model_df2)
}


output_stoch_model2 <- loop_thru_vaxs2()

  # note the policy choice
output_stoch_model2$policy_choice <- "Offered free of charge"

  # bind both outputs into one df
output_stoch_model <- rbind(output_stoch_model1, output_stoch_model2)

# remove objects we wont need downstream from memory
rm(list=setdiff(ls(), c("east_africa_shp",  "output_stoch_model", "output_det_model",
                "vax_covs")))





