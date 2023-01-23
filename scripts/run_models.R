

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
               here)


# read shapefile #####
east_africa_shp <- st_read(dsn="./shapefiles/", layer="ea_shapefile")
east_africa_shp$Population <- as.numeric(east_africa_shp$Population) 


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
                                             P_initiate_PEP_rabid_bite = 0.6,       # of rabid bites (`a further 15% did not obtain PEP`)
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
      #my_list <- append(my_list, vax_cov)
    }
    # create new row to merge projections per district
    df1 <- data.frame(my_list)
    colnames(df1) = unlist(lapply(names(model_output[[1]]), create_new_names), recursive = FALSE)
    #df1$vax_cov <- vax_cov
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
                           HDR_min=4, HDR_max=40, 
                           vax_cov=vax_cov,
                           inc_min=0.05, inc_max=0.1, 
                           P_bite_rabid=0.38, 
                           P_seek_care_rabid_bite=0.75,
                           P_initiate_PEP_rabid_bite=0.6, 
                           P_complete_PEP_rabid_bite=0.473,
                           P_seek_care_healthy_bite=0.78,
                           P_initiate_PEP_healthy_bite=0.2, 
                           P_complete_PEP_healthy_bite=0.07,
                           P_death=0.17, 
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
                           HDR_min=10, HDR_max=40, 
                           vax_cov=vax_cov,
                           inc_min=0.05, inc_max=0.1, 
                           P_bite_rabid=0.38, 
                           P_seek_care_rabid_bite=0.75,     # not clear from Changalucha et al., (using value for IF patient pays/ status quo)
                           P_initiate_PEP_rabid_bite=0.899, 
                           P_complete_PEP_rabid_bite=0.542,
                           # No data here there4 using same values for healthy bites despite PEP policy choice
                           P_seek_care_healthy_bite=0.78,
                           P_initiate_PEP_healthy_bite=0.2, 
                           P_complete_PEP_healthy_bite=0.07,
                           P_death=0.17, 
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





