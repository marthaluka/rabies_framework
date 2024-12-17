
rm(list=ls())

# Decision tree model that can be applied to create different scenarios 

decision_tree <- function(N, pop, HDR, horizon, discount, #LR_range, 
                          mu, k, pSeek_healthy, pBite_healthy,
                          pStart_healthy, pComplete_healthy, pSeek_exposure,
                          pStart_exposure, pComplete_exposure, pDeath, pPrevent, 
                          full_cost, partial_cost, mdv_campaign_budget = NULL, base_vax_cov,
                          vaccinate_dog_cost, target_vax_cov = NULL, 
                          pInvestigate, pFound,  pTestable, pFN) {

  # workaround predictions when horizon is 1
         # Initialize only_a_single_year to a default value
  only_a_single_year <- 'no'
  
  if (horizon == 1){
    horizon = 2
    only_a_single_year = 'yes'
  }
  
    
  # source helper functions  
  source("./scripts/HelperFun.R")
  
  # SIMULATE TIMESERIES
  
  # Estimate dog population
  HDR <- runif(n=N, min = HDR[1], max = HDR[2])  # Explore uncertainty in HDR - uniform distribution w/ upper & lower limits 
  
  dog_pop <- matrix(NA,nrow=N,ncol=horizon)
  for (year in 1:horizon){
    dog_pop[,year] <- pop/HDR 
  }
  
  
  # Vaccination coverage calculation based on input
  if (!is.null(mdv_campaign_budget)) {
    # Use the budget-based function to calculate vaccination coverage
    vax_cov <- vax_coverage_from_budget(
      mdv_campaign_budget = mdv_campaign_budget,
      base_vax_cov = base_vax_cov,
      vaccinate_dog_cost = vaccinate_dog_cost,  
      dog_pop = dog_pop, 
      horizon = horizon,
      discount = discount
    )
    
    # Notify user that budget overwrites target coverage
    message("Overwriting target_vax_cov with budget approach. To use target_vax_cov, set budget as NULL")
    
  } else if (!is.null(target_vax_cov)) {
    # Use target_vax_cov if budget is not provided
    vax_cov <- vax_coverage_over_x_years(base_vax_cov, target_vax_cov, horizon)
  } else {
    stop("Either target_vax_cov or mdv_campaign_budget must be provided.")
  }
  
  
  # dogs vaccinated
  ts_dogs_vaccinated <- matrix(NA,nrow=N,ncol=horizon)
  for (year in 1:horizon){
    ts_dogs_vaccinated[,year] <- dog_pop[,year] * vax_cov[[year]]
  }
  
  # susceptible dogs  
  sus_dogs <- matrix(NA,nrow=N,ncol=horizon)
  for (year in 1:horizon){
    sus_dogs[,year] <- dog_pop[,year] - ts_dogs_vaccinated[,year]
  }
  
  
  # MDV campaign cost 
  MDV_campaign_cost <- matrix(NA, nrow=N, ncol=horizon)
  
  if (!is.null(mdv_campaign_budget)) {
    # If budget is provided, assign it directly to each year
    for (year in 1:horizon) {
      MDV_campaign_cost[, year] <- mdv_campaign_budget
    }
  } else {
    # If budget is not provided, calculate costs based on vaccinated dogs
    for (year in 1:horizon) {
      MDV_campaign_cost[, year] <- ts_dogs_vaccinated[, year] * 
        runif(n=N, min=vaccinate_dog_cost[1], max=vaccinate_dog_cost[2])
    }
  }
  
  
  ts_rabid_dogs<- predict_cases(nreps=N, vax_cov = vax_cov, horizon = horizon, dog_pop= dog_pop, rabies_inc=rabies_inc)
  
  
  # Rabid bites
  ## Exposures from times series of rabid dogs
  ts_exposures <- matrix(NA,nrow=N,ncol=horizon)
  ts_rabid_biting_dogs <- matrix(NA,nrow=N,ncol=horizon)
  
  for (year in seq(1,horizon)){
    output <- sapply(FUN = nBitesBiters, pBite = mu, pBiteK = k, X = ts_rabid_dogs[,year]) 
    ts_exposures[,year] <- unlist(output[1,])       # nBites
    ts_rabid_biting_dogs[,year] <- unlist(output[2,])    # nBiters
  }
  
  ## IBCM
  
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
  
  # Add probability that a healthy animal bite is flagged as potentially suspicious (0.05)
  # false positives         #to do
  # not sure if the denominator should be ts_rabid_biting_investigated (I now think this should be ts_healthy_biting_dogs)# Chat with Elaine
  ts_healthy_biting_investigated <- matrix(NA,nrow=N,ncol=horizon)
  for (year in 1:horizon){
    ts_healthy_biting_investigated[,year] <- rbinom(n=N,  size = ts_healthy_bites[,year], prob = pFN)
  }
  
  
  
  # Healthy biting dogs ########
      # to do                     # Chat with Elaine

  # Persons bitten by healthy dogs
  # healthy_bites <- sim_patient_ts(pop, inc_range = LR_range, horizon)
  # healthy_bites <- round(rgamma(N, shape=6.675, rate=2889.090)* dog_pop) 
  
  
  # Healthy biting dogs that are found
  ts_healthy_biting_found <- matrix(NA,nrow=N,ncol=horizon)
  for (year in 1:horizon){
    ts_healthy_biting_found[,year] <- rbinom(n=N,  size = ts_healthy_biting_investigated[,year], prob = pFound)
  }
  
  
  # HEALTHCARE SEEKING 
  
  # time series ts_exposures_seek_care
  ts_exposures_seek_care <- matrix(nrow = N, ncol = horizon)
  for (year in seq(1, horizon)){
    ts_exposures_seek_care[,year] <- rbinom(n=N,  size = ts_exposures[,year], prob = pSeek_exposure)
  }
  
  # time series ts_exposures_do_not_seek_care
  ts_exposures_do_not_seek_care <- ts_exposures - ts_exposures_seek_care
  
  
  # Rabid bite victims:
  
  ts_exp_start <- matrix(nrow = N, ncol = horizon)
  ts_exp_complete <- matrix(nrow = N, ncol = horizon)
  for (year in seq(1, horizon)){
    # time series start PEP
    ts_exp_start[,year] <- rbinom(n=N,  size = ts_exposures_seek_care[,year], prob = pStart_exposure)
    # time series complete PEP
    ts_exp_complete[,year] <- rbinom(n=N,  size = ts_exp_start[,year], prob = pComplete_exposure)
  }
  
  
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
  
  ts_exposures_seek_care_inc = (ts_exposures_seek_care/pop)*1E5        # per 100k
  ts_healthy_seek_care_inc = (ts_healthy_seek_care/pop)*1E5
  
  # pStart
  ts_healthy_start <- matrix(nrow = N, ncol = horizon)
  ts_healthy_complete <- matrix(nrow = N, ncol = horizon)
  for (year in seq(1, horizon)){
    # time series start PEP
    ts_healthy_start[,year] <- rbinom(n=N,  size = ts_healthy_seek_care[,year], prob = pStart_healthy)
    # time series complete PEP
    ts_healthy_complete[,year] <- rbinom(n=N,  size = ts_healthy_start[,year], prob = pComplete_healthy)
  }
  
  ts_healthy_no_start <- (ts_healthy_seek_care - ts_healthy_start) + ts_healthy_do_not_seek_care
  ts_healthy_incomplete <- ts_healthy_start - ts_healthy_complete
  
  
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
  
  
  # PEP IMPACTS (because we can see who got PEP)
  
  deaths_averted_PEP_complete <- matrix(nrow = N, ncol = horizon)
  deaths_averted_PEP_incomplete <- matrix(nrow = N, ncol = horizon)
  for (year in seq(1, horizon)){
    # deaths averted complete PEP
    deaths_averted_PEP_complete[,year] <- rbinom(n=N,  size = ts_exp_complete[,year], prob = pDeath) 
    # deaths incomplete PEP
    deaths_averted_PEP_incomplete[,year] <- rbinom(n=N,  size = (ts_exp_incomplete[,year] - deaths_incomplete_PEP[,year]), prob = pPrevent * pDeath)
  }
  
  ts_deaths_averted_PEP <-  deaths_averted_PEP_complete + deaths_averted_PEP_incomplete
  
  # MDV IMPACTS ()
  ######
  
  # Run only if mdv_campaign_budget is provided (and non-zero) or target_vax_cov > base_vax_cov
  if ((!is.null(mdv_campaign_budget) && mdv_campaign_budget != 0) || 
      (target_vax_cov > base_vax_cov)) {

    vax_cov_no_MDV <- vax_coverage_over_x_years(base_vax_cov, 0, horizon) # target coverage is 0 as there are no efforts
    
    # Predict rabid dogs without MDV
    ts_rabid_dogs_no_MDV <- predict_cases(
      nreps=N, 
      vax_cov=vax_cov_no_MDV, 
      horizon=horizon, 
      dog_pop=dog_pop, 
      rabies_inc=rabies_inc
    )
    
    # Predict human exposures without MDV
    ts_exposures_no_MDV <- matrix(NA, nrow=N, ncol=horizon)
    for (year in seq(1, horizon)){
      output_no_MDV <- sapply(FUN = nBitesBiters, pBite=mu, pBiteK=k, X=ts_rabid_dogs_no_MDV[,year]) 
      ts_exposures_no_MDV[,year] <- unlist(output_no_MDV[1,])  # nBites
    }
    
    # Deaths without PEP in no-MDV scenario
    ts_deaths_no_MDV <- matrix(nrow=N, ncol=horizon)
    for (year in seq(1, horizon)){
      ts_deaths_no_MDV[,year] <- rbinom(n=N, size=ts_exposures_no_MDV[,year], prob=pDeath)
    }
    
    # Deaths averted by MDV
    ts_deaths_averted_MDV <- ts_deaths_no_MDV - ts_deaths
    ts_deaths_averted_MDV <- pmax(ts_deaths_no_MDV - ts_deaths, 0) # forcing negatives to zero
  }

  ######
  
  
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
  
  if (only_a_single_year == 'yes'){
    # get the matrices 
    out_matrices <- lapply(my_list, function(mat) get(mat)[, 1, drop = FALSE])
  }else{
    out_matrices <- lapply(my_list, function(mat) get(mat))
  }
  
  # name
  names(out_matrices) <- my_list
  
  # output
  return(out_matrices)
  
}



decision_tree(N=10, pop=1000000, HDR=c(10,20), horizon=7, discount=0.03,#LR_range, 
              mu=0.38, k=0.14, pSeek_healthy=0.78,pBite_healthy=0.01,
              pStart_healthy=0.2, pComplete_healthy=0.20, pSeek_exposure=0.75,
              pStart_exposure=0.899, pComplete_exposure=0.542, pDeath=0.166, pPrevent=0.986, 
              full_cost =45, partial_cost=25, mdv_campaign_budget=100000, base_vax_cov=0.05,
              vaccinate_dog_cost=2, target_vax_cov=0.7, #campaign_budget
              pInvestigate=0.9, pFound=0.6,  pTestable=0.7, pFN=0.05
) 











