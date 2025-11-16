



library(tidyverse)
library(brms)

source("./scripts/HelperFun.R")
decision_tree2 <- function(pop = 100000, HDR = c(10,20), horizon = 5, discount = 0.03, mu = 0.38, k = 0.14, 
                          pSeek_healthy = 0.2, pBite_healthy = 0.2, pStart_healthy = 0.1, pComplete_healthy = 0.1, 
                          pSeek_exposure = 0.78, pStart_exposure = 0.7, pComplete_exposure = 0.6, pDeath = 0.16, 
                          pPrevent = 0.99, full_cost = 45, partial_cost = 25, mdv_campaign_budget = NULL , 
                          base_vax_cov = 0.05, vaccinate_dog_cost = c(2,4), target_vax_cov = 0.4 , pInvestigate = 0.5, 
                          pFound = 0.4, pTestable = 0.2, pFP = 0.05){
  
  # Step 1: Initialize horizon settings
  only_a_single_year <- initialize(horizon)
  
  # Step 2: Estimate dog population
  hdr = mean(c(HDR[1], HDR[2]))
  dog_pop <- matrix(NA, nrow = 1, ncol = horizon)
  for (year in 1:horizon) {
    dog_pop[, year] <- pop / hdr
  }
  
  # Step 3: Calculate vaccination coverage
  vax_cov <- calculate_vax_coverage(mdv_campaign_budget, base_vax_cov, vaccinate_dog_cost, dog_pop, horizon, target_vax_cov)
  
  # Step 4: Calculate vaccinated and susceptible dog populations
  vax_results <- calculate_vaccinated_and_susceptible(N=1, horizon, dog_pop, vax_cov)
  
  # Step 5: Calculate MDV campaign costs
  calculate_campaign_cost2 <- function(horizon, mdv_campaign_budget, ts_dogs_vaccinated, vaccinate_dog_cost) {
    MDV_campaign_cost <- matrix(NA, nrow = 1, ncol = horizon)
    
    if (!is.null(mdv_campaign_budget)) {
      for (year in 1:horizon) {
        MDV_campaign_cost[, year] <- mdv_campaign_budget
      }
    } else {
      for (year in 1:horizon) {
        MDV_campaign_cost[, year] <- ts_dogs_vaccinated[, year] *
          mean(c(vaccinate_dog_cost[1], vaccinate_dog_cost[2]))
      }
    }
    return(MDV_campaign_cost)
  }
  
  
  MDV_campaign_cost <- calculate_campaign_cost2(horizon, mdv_campaign_budget, vax_results$ts_dogs_vaccinated, vaccinate_dog_cost)
  
  # Step 6A: Predict rabid dogs and human exposures
  
  nBitesBiters_deterministic <- function(dog_pop, pBite) {
    nBites <- dog_pop * pBite              # Expected total number of bites
    nBiters <- dog_pop * (1 - exp(-pBite)) # Expected number of biting dogs
    
    return(list(nBites = nBites, nBiters = nBiters))
  }
  
  predict_rabies2 <- function(horizon, vax_cov, dog_pop, rabies_inc, mu) {
    ts_rabid_dogs <- predict_cases(nreps = 1, vax_cov = vax_cov, horizon = horizon, dog_pop = dog_pop, rabies_inc = rabies_inc)
    ts_exposures <- matrix(NA, nrow = 1, ncol = horizon)
    ts_rabid_biting_dogs <- matrix(NA, nrow = 1, ncol = horizon)
    
    for (year in seq(1, horizon)) {
      output <- nBitesBiters_deterministic(ts_rabid_dogs[, year], mu)
      ts_exposures[, year] <- output$nBites
      ts_rabid_biting_dogs[, year] <- output$nBiters
    }
    
    return(list(ts_rabid_dogs = ts_rabid_dogs, ts_exposures = ts_exposures, ts_rabid_biting_dogs = ts_rabid_biting_dogs))
  }
  
  rabies_results <- predict_rabies2(horizon, vax_cov, dog_pop, rabies_inc, mu)
  
  
  # Step 6B:  Healthy bites
  ts_healthy_bites <- dog_pop * pBite_healthy
  
  
  # Step 7: Investigate rabid and healthy bites
    ts_rabid_biting_investigated <- rabies_results$ts_rabid_biting_dogs * pInvestigate
    ts_rabid_biting_found <- ts_rabid_biting_investigated * pFound
    ts_rabid_biting_testable <- ts_rabid_biting_found * pTestable


  bite_investigation_results <- list(
    ts_rabid_biting_investigated = ts_rabid_biting_investigated,
    ts_rabid_biting_found = ts_rabid_biting_found,
    ts_rabid_biting_testable = ts_rabid_biting_testable
  )
  
  
  
  # Step 8: Calculate healthcare-seeking behavior
    ts_exposures_seek_care <- rabies_results$ts_exposures * pSeek_exposure
    ts_exp_start <- ts_exposures_seek_care * pStart_exposure
    ts_exp_complete <- ts_exp_start * pComplete_exposure
    
    ts_healthy_seek_care <- ts_healthy_bites * pSeek_healthy
    ts_healthy_start <- ts_healthy_seek_care * pStart_healthy
    ts_healthy_complete <- ts_healthy_start * pComplete_healthy

  
  
  healthcare_results <- list(ts_exp_seek_care = ts_exposures_seek_care,
                             ts_exp_start = ts_exp_start,
                             ts_exp_complete = ts_exp_complete,
                             
                             ts_healthy_seek_care = ts_healthy_seek_care,
                             ts_healthy_start = ts_healthy_start,
                             ts_healthy_complete = ts_healthy_complete)
  
  
  
  # Step 9: Predict deaths
  # some calcs first
  ts_exp_no_start = rabies_results$ts_exposures - healthcare_results$ts_exp_start
  ts_exp_incomplete = healthcare_results$ts_exp_start - healthcare_results$ts_exp_complete
  
  #  function
  predict_deaths_and_deathsaverted_PEP2 <- function(horizon, ts_exposures, ts_exp_no_start, ts_exp_complete, ts_exp_incomplete, pDeath, pPrevent) {
    
    ts_deaths_no_PEP <- ts_exp_no_start * pDeath
    deaths_incomplete_PEP <- ts_exp_incomplete * (1 - pPrevent)
      # deaths averted 
    deaths_averted_PEP_complete <- ts_exp_complete * pPrevent
    deaths_averted_PEP_incomplete <- ts_exp_incomplete * (pPrevent * pDeath)
      
    
    ts_deaths <- ts_deaths_no_PEP + deaths_incomplete_PEP
    ts_deaths_averted_PEP <- deaths_averted_PEP_complete + deaths_averted_PEP_incomplete
    
    return(list(ts_deaths = ts_deaths,
                ts_deaths_averted_PEP = ts_deaths_averted_PEP)
    )
  }
  
  
  deaths_PEP <- predict_deaths_and_deathsaverted_PEP2(horizon, rabies_results$ts_exposures, ts_exp_no_start, 
                                                     healthcare_results$ts_exp_complete, ts_exp_incomplete, pDeath, pPrevent)
  
  # Step 10:  deaths averted by MDV
  
  compute_deaths_averted_MDV2 <- function(horizon, mdv_campaign_budget, target_vax_cov, base_vax_cov, 
                                         dog_pop, rabies_inc, mu, pDeath, ts_deaths) {
    
    # Initialize outputs
    ts_deaths_averted_MDV <- NULL
    ts_deaths_no_MDV <- NULL
    
    # Run only if MDV budget is non-zero or if target coverage is greater than base coverage
    if ((!is.null(mdv_campaign_budget) && mdv_campaign_budget != 0) || 
        (target_vax_cov > base_vax_cov)) {
      
      # Expected vaccination coverage with no MDV effort
      vax_cov_no_MDV <- vax_coverage_over_x_years(base_vax_cov, 0, horizon) # No MDV efforts
      
      # Predict rabid dogs without MDV (expected number)
      ts_rabid_dogs_no_MDV <- predict_cases(
        nreps = 1, 
        vax_cov = vax_cov_no_MDV, 
        horizon = horizon, 
        dog_pop = dog_pop, 
        rabies_inc = rabies_inc
      )
      # Compute expected exposures using the expected number of bites
      ts_exposures_no_MDV <- ts_rabid_dogs_no_MDV * mu  # Expected number of bites
      
      # Expected deaths without MDV
      ts_deaths_no_MDV <- ts_exposures_no_MDV * pDeath  # Deterministic deaths
      
      # Compute deaths averted due to MDV
      ts_deaths_averted_MDV <- pmax(ts_deaths_no_MDV - ts_deaths, 0)  # Ensuring no negative values
    }
    
    return(list(
      ts_deaths_averted_MDV = ts_deaths_averted_MDV
    ))
  }
  
  
  
  ts_deaths_averted_MDV <- compute_deaths_averted_MDV2(horizon, mdv_campaign_budget, target_vax_cov, base_vax_cov, 
                                                      dog_pop, rabies_inc, mu, pDeath, deaths_PEP$ts_deaths)
  
  
  
  
  
  # Step 10: Collect all variables starting with "ts_"
  all_results <- c(vax_results, rabies_results, bite_investigation_results, healthcare_results, deaths_PEP, ts_deaths_averted_MDV,
                   list(
                     MDV_campaign_cost = MDV_campaign_cost,
                     ts_healthy_bites = ts_healthy_bites))
  
  
  # If only a single year was requested, extract the first column of each matrix
  if (only_a_single_year == 'yes') {
    all_results <- lapply(all_results, function(mat) mat[, 1, drop = FALSE])
  }
  
  
  # output
  return(all_results)
  
}





decision_tree2(pop = 600000, HDR = c(25,35), horizon = 5, mu = 0.38, k = 0.14, 
              pBite_healthy = 0.01, pSeek_healthy = 0.78, pStart_healthy = 0.2, pComplete_healthy = 0.07, 
              pSeek_exposure = 0.7, pStart_exposure = 0.79, pComplete_exposure = 0.473, pDeath = 0.1660119, 
              pPrevent = 0.986, full_cost = 45, partial_cost = 25, mdv_campaign_budget = NULL , 
              base_vax_cov = 0.05, vaccinate_dog_cost = c(2,4), target_vax_cov = 0, pInvestigate = 0.9, 
              pFound = 0.6, pTestable = 0.7, pFP = 0.05)






