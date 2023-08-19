
rm(list=ls())

# Decision tree model that can be applied to create different scenarios 

decision_tree <- function(N, pop, HDR, horizon, discount,#LR_range, 
                          mu, k, pSeek_healthy,pBite_healthy,
                          pStart_healthy, pComplete_healthy, pSeek_exposure,
                          pStart_exposure, pComplete_exposure, pDeath, pPrevent, 
                          full_cost, partial_cost, campaign_cost, base_vax_cov,
                          vaccinate_dog_cost, target_vax_cov, #campaign_budget
                          pInvestigate, pFound,  pTestable, pFN
                          ) {

  
  
# source helper functions  
source("./scripts/HelperFun.R")
  
# SIMULATE TIMESERIES

# Estimate dog population
HDR <- runif(n=N, min = HDR[1], max = HDR[2])  # Explore uncertainty in HDR - uniform distribution w/ upper & lower limits 
dog_pop <- pop/HDR 

# # Vaccination coverage either from target or budget
# vax_cov <- vax_coverage_over_x_years(0.05, 0.7, horizon)
# # Or
# vax_cov2 <- vax_coverage_from_budget(50000, 0.05, horizon, 20000, 2, discount)

vax_cov <- vax_coverage_over_x_years(base_vax_cov, target_vax_cov, horizon)


# dogs vaccinated
ts_dogs_vaccinated <- matrix(NA,nrow=N,ncol=horizon)
for (year in 1:horizon){
  ts_dogs_vaccinated[,year] <- dog_pop * vax_cov[[year]]
}

# susceptible dogs  
sus_dogs <- matrix(NA,nrow=N,ncol=horizon)
for (year in 1:horizon){
  sus_dogs[,year] <- dog_pop - ts_dogs_vaccinated[,year]
}


# MDV campaign cost 
  # Not necessary if input was a set budget
MDV_campaign_cost <- matrix(NA,nrow=N,ncol=horizon)
for (year in 1:horizon){
  MDV_campaign_cost[,year] <- ts_dogs_vaccinated[,year] * (runif(n=N, min = vaccinate_dog_cost[1], max = vaccinate_dog_cost[2]))
}


ts_rabid_dogs<- predict_cases(nreps=N, vax_cov = vax_cov, horizon =horizon, dog_pop= dog_pop, rabies_inc=rabies_inc)


# Rabid bites
## Exposures from times series of rabid dogs
ts_exposures <- matrix(NA,nrow=N,ncol=horizon)
for (year in seq(1,horizon)){
  ts_exposures[,year] <- sapply(FUN = nBites, pBite = mu, pBiteK = k, X = ts_rabid_dogs[,year]) 
}

## IBCM
# Rabid biting dogs
ts_rabid_biting_dogs <- matrix(NA,nrow=N,ncol=horizon)
for (year in 1:horizon){
  ts_rabid_biting_dogs[,year] <- sapply(FUN = nBiters, pBite = mu, pBiteK = k, X = ts_rabid_dogs[,year]) 
}


# Rabid biting dogs that are investigated
ts_rabid_biting_investigated <- matrix(NA,nrow=N,ncol=horizon)
for (year in 1:horizon){
  ts_rabid_biting_investigated[,year] <- rbinom(n=N,  size = ts_rabid_biting_dogs[,year], prob = pInvestigate)
}

# Rabid biting dogs that are found
ts_rabid_biting_found <- matrix(NA,nrow=N,ncol=horizon)
for (year in 1:horizon){
  ts_rabid_biting_found[,year] <- rbinom(n=N,  size = ts_rabid_biting_investigated[,year], prob = pFound)
}

# Rabid biting dogs that are testable
ts_rabid_biting_testable <- matrix(NA,nrow=N,ncol=horizon)
for (year in 1:horizon){
  ts_rabid_biting_testable[,year] <- rbinom(n=N,  size = ts_rabid_biting_found[,year], prob = pTestable)
}


# Add probability that a healthy animal bite is flagged as potentially suspicious (0.05)
# false positives
 # not sure if the denominator should be ts_rabid_biting_found or ts_rabid_biting_investigated
ts_healthy_FP <- matrix(NA,nrow=N,ncol=horizon)
for (year in 1:horizon){
  ts_healthy_FP[,year] <- rbinom(n=N,  size = ts_rabid_biting_found[,year], prob = pFN)
}




# Create patient time series for a given population 
# consider using IBCM data and running for both low- and high-risk bite patient incidence 
# for range of bite incidence (high or low risk!) simulate bite patient time series 


# Healthy bites #####
  # Using LR_range --to review if still in use for IBCM
# sim_patient_ts = function(pop, inc_range, horizon){
#   inc <- runif(horizon, min = inc_range[1], max = inc_range[2]) # select incidence each year over time horizon
#   # open matrix for output
#   ts_healthy_bites <- matrix(nrow = N, ncol = horizon)
#   #loop through years
#   for (year in seq(1, horizon)){
#     ts_healthy_bites[,year] <- unlist(lapply(FUN = rpois, n=N, X = inc[year]/100000 * pop)) # convert incidence into bite patients per year for population
#   }
#   return(ts_healthy_bites) 
# }

ts_healthy_bites <- matrix(nrow = N, ncol = horizon)
for (year in seq(1, horizon)){
  ts_healthy_bites[,year] <- rbinom(n=N,  size = round(dog_pop), prob = pBite_healthy)
}


# Persons bitten by healthy dogs
# healthy_bites <- sim_patient_ts(pop, inc_range = LR_range, horizon)
    # healthy_bites <- round(rgamma(N, shape=6.675, rate=2889.090)* dog_pop) 

# pSEEK for rabid bites
# exposures_seek_care <- rbinom(n=N,  size = exposures, prob = pSeek_exposure)
# exposures_do_not_seek_care <- exposures - exposures_seek_care

# time series ts_exposures_seek_care
ts_exposures_seek_care <- matrix(nrow = N, ncol = horizon)
for (year in seq(1, horizon)){
  ts_exposures_seek_care[,year] <- rbinom(n=N,  size = ts_exposures[,year], prob = pSeek_exposure)
}

# time series ts_exposures_do_not_seek_care
ts_exposures_do_not_seek_care <- ts_exposures - ts_exposures_seek_care


# To do #######
# PEP USE: based on health seeking (probabilities depend on PEP policies e.g. if free or charged


# Rabid bite victims:

ts_exp_start <- matrix(nrow = N, ncol = horizon)
ts_exp_complete <- matrix(nrow = N, ncol = horizon)
for (year in seq(1, horizon)){
  # time series start PEP
  ts_exp_start[,year] <- rbinom(n=N,  size = ts_exposures_seek_care[,year], prob = pStart_exposure)
  # time series complete PEP
  ts_exp_complete[,year] <- rbinom(n=N,  size = ts_exp_start[,year], prob = pComplete_exposure)
}

# for (year in seq(1, horizon)){
#   # time series start PEP
#   ts_exp_start[,year] <- unlist(lapply(FUN = rbinom, n=N, prob = pStart_exposure, X = ts_exposures_seek_care[,year]))
#   # time series complete PEP
#   ts_exp_complete[,year] <- unlist(lapply(FUN = rbinom, n=N, prob = pComplete_exposure, X = ts_exp_start[,year]))
# }

ts_exp_no_start <- (ts_exposures_seek_care - ts_exp_start) + ts_exposures_do_not_seek_care
ts_exp_incomplete <- ts_exp_start - ts_exp_complete


# Healthy bites

# pSEEK for healthy bites

# time series ts_exposures_seek_care
ts_healthy_seek_care <- matrix(nrow = N, ncol = horizon)
for (year in seq(1, horizon)){
  ts_healthy_seek_care[,year] <- rbinom(n=N,  size = ts_healthy_bites[,year], prob = pSeek_healthy)
}

# time series ts_exposures_do_not_seek_care
ts_healthy_do_not_seek_care <- ts_healthy_bites - ts_healthy_seek_care


# IBCM ######
# Total bite victims seeking care
total_seek_care = ts_exposures_seek_care + ts_healthy_seek_care
ts_total_seek_care_inc = (total_seek_care/pop)*1E5

ts_exposures_seek_care_inc = (ts_exposures_seek_care/pop)*1E5
ts_healthy_seek_care_inc = (ts_healthy_seek_care/pop)*1E5

# pStart
healthy_start <- matrix(nrow = N, ncol = horizon)
ts_healthy_complete <- matrix(nrow = N, ncol = horizon)
for (year in seq(1, horizon)){
  # time series start PEP
  healthy_start[,year] <- rbinom(n=N,  size = ts_healthy_seek_care[,year], prob = pStart_healthy)
  # time series complete PEP
  ts_healthy_complete[,year] <- rbinom(n=N,  size = healthy_start[,year], prob = pComplete_healthy)
}

healthy_no_start <- (ts_healthy_seek_care - healthy_start) + ts_healthy_do_not_seek_care
ts_healthy_incomplete <- healthy_start - ts_healthy_complete


# DEATHS

# pDeath
ts_deaths_no_PEP <- matrix(nrow = N, ncol = horizon)
deaths_incomplete_PEP <- matrix(nrow = N, ncol = horizon)
for (year in seq(1, horizon)){
  # deaths no PEP
  ts_deaths_no_PEP[,year] <- rbinom(n=N,  size = ts_exp_no_start[,year], prob = pDeath)
  # deaths incomplete PEP
  deaths_incomplete_PEP[,year] <- rbinom(n=N,  size = ts_exp_incomplete[,year], prob = 1-pPrevent)
}

ts_deaths <- ts_deaths_no_PEP + deaths_incomplete_PEP


# PEP IMPACTS (because we can see who got PEP!)

deaths_averted_PEP_complete <- matrix(nrow = N, ncol = horizon)
deaths_averted_PEP_incomplete <- matrix(nrow = N, ncol = horizon)
for (year in seq(1, horizon)){
  # deaths averted complete PEP
  deaths_averted_PEP_complete[,year] <- rbinom(n=N,  size = ts_exp_complete[,year], prob = pDeath) 
  # deaths incomplete PEP
  deaths_averted_PEP_incomplete[,year] <- rbinom(n=N,  size = (ts_exp_incomplete[,year] - deaths_incomplete_PEP[,year]), prob = pPrevent * pDeath)
}

ts_deaths_averted_PEP <-  deaths_averted_PEP_complete + deaths_averted_PEP_incomplete

# deaths_averted_PEP <- unlist(lapply(FUN = rbinom, n=N, prob = pDeath, X = ts_exp_complete[,1])) # pDeath ??
# deaths_averted_PEP_incomplete <- unlist(lapply(FUN = rbinom, n=N, prob = pPrevent * pDeath, X = exp_incomplete - deaths_incomplete_PEP))
# deaths_averted <-  deaths_averted_PEP + deaths_averted_PEP_incomplete


# PEP DELIVERED
ts_complete_PEP <- ts_exp_complete + ts_healthy_complete
ts_incomplete_PEP <- ts_exp_incomplete + ts_healthy_incomplete

# Economics #####
  ## Discount costs
future <- (1:horizon)-1
ts_cost_PEP_per_year <- ((ts_complete_PEP * full_cost) + (ts_incomplete_PEP * partial_cost)) * exp(-discount*future)
ts_MDV_campaign_cost <- MDV_campaign_cost * exp(-discount*future)

  ## Add
ts_cost_per_year <- ts_cost_PEP_per_year + ts_MDV_campaign_cost 


# list all ts outputs
my_list<- ls(pattern="ts_")
# get the matrices 
out_matrices <- lapply(my_list, function(mat) get(mat))
# name
names(out_matrices) <- my_list

# output
return(out_matrices)

}





