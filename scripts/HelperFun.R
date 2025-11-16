

# uses either a target coverage OR budget OR both. 
## If both are provided: Target is the “best‐case” goal & Budget is the “reality check” that overrides that goal if funds aren’t enough
## If budget is "endless", we achieve our target

calc_vax_coverage <- function(base_vax_cov, target_vax_cov = NULL, mdv_campaign_budget = NULL, 
                              vaccinate_dog_cost = NULL, dog_pop = NULL, horizon, discount = 0) {
  
  # Input validation
  if (is.null(target_vax_cov) && is.null(mdv_campaign_budget)) {
    stop("At least one of 'target_vax_cov' or 'mdv_campaign_budget' must be provided.")
  }
  
  # Use mean cost if a vector is provided
  cost_per_dog <- if (length(vaccinate_dog_cost) > 1) mean(vaccinate_dog_cost) else vaccinate_dog_cost
  
  # Cap target at 0.8 if provided
  if (!is.null(target_vax_cov)) {
    target_vax_cov <- min(target_vax_cov, 0.8)
  }
  
  # Helper: generate non-linear ramp-up to final coverage
  generate_vax_coverage <- function(final_coverage) {
    vax_coverage_list <- numeric(horizon)
    previous_coverage <- base_vax_cov
    for (year in 1:horizon) {
      if (year <= 3) {
        vax_coverage_list[year] <- previous_coverage + (final_coverage - previous_coverage) * (year / 3)
      } else {
        vax_coverage_list[year] <- final_coverage
      }
      previous_coverage <- vax_coverage_list[year]
    }
    vax_coverage_list <- c(base_vax_cov, vax_coverage_list[-horizon])
    return(vax_coverage_list)
  }
  
  # Case 1: Only target provided
  if (!is.null(target_vax_cov) && is.null(mdv_campaign_budget)) {
    return(generate_vax_coverage(target_vax_cov))
  }
  
  # Case 2: Only budget provided
  if (!is.null(mdv_campaign_budget) && is.null(target_vax_cov)) {
    if (is.null(cost_per_dog) || is.null(dog_pop)) {
      stop("Both 'vaccinate_dog_cost' and 'dog_pop' must be provided.")
    }
    max_achievable <- min(floor(mdv_campaign_budget / cost_per_dog) / dog_pop, 0.8)
    return(generate_vax_coverage(max_achievable))
  }
  
  # Case 3: Both target and budget provided
  if (!is.null(target_vax_cov) && !is.null(mdv_campaign_budget)) {
    if (is.null(cost_per_dog) || is.null(dog_pop)) {
      stop("Both 'vaccinate_dog_cost' and 'dog_pop' must be provided.")
    }
    max_achievable <- min(floor(mdv_campaign_budget / cost_per_dog) / dog_pop, 0.8)
    
    if (max_achievable >= target_vax_cov) {
      return(generate_vax_coverage(target_vax_cov))
    } else {
      message("Budget is insufficient to achieve target coverage; using budget-based maximum coverage.")
      return(generate_vax_coverage(max_achievable))
    }
  }
}


# 
# 
# # Vaccination coverage attained given a certain target coverage
# vax_coverage_over_x_years <- function(base_vax_cov, target_vax_cov, horizon){
#   # Linear interpolation between initial and target vaccination coverage over 3 years
#   # # assume it takes 3 years to hit target vaccination coverage
#   interpolated_values <- map2(base_vax_cov, target_vax_cov, seq, length.out = 3)
#   
#   # If horizon is set to less than 3 years
#   if (horizon < 3){
#     annual_vax_cov = interpolated_values[[1]][c(1:horizon)]
#   } else {
#     sustained_values <- rep(target_vax_cov, (horizon - 3))
#     annual_vax_cov <- unlist(append(interpolated_values, sustained_values))
#   }
#   
#   # Return random values between target and base_vax_cov if target is 0
#   if(target_vax_cov == 0){
#     return(runif(n = horizon, min = target_vax_cov, max = base_vax_cov*1.2))
#   } else {
#     return(annual_vax_cov)
#   }
# }
# 
# 
# # modifying this to take set budget as input- in place of target coverage 
# vax_coverage_from_budget <- function(mdv_campaign_budget, base_vax_cov, vaccinate_dog_cost, dog_pop, horizon){
#   
#   vax_coverage_list <- numeric(horizon) # initialize an empty numeric vector of length 'horizon'
#   
#   previous_coverage <- base_vax_cov
#   for(year in 1:horizon){
#     # Extract the budget for the current year
#     budget <- mdv_campaign_budget
#     
#     # Calculate number of dogs that can be vaccinated with the budget
#     dogs_vaccinated <- floor(budget / vaccinate_dog_cost)
#     
#     # Calculate the maximum potential coverage based on the  budge
#     potential_coverage <- min(dogs_vaccinated / dog_pop, 0.8)
#     
#     # Gradually increase the coverage over three years
#     if (year <= 3) {
#       vax_coverage_list[year] <- previous_coverage + (potential_coverage - previous_coverage) * (year/3)
#     } else {
#       vax_coverage_list[year] <- potential_coverage
#     }
#     
#     # Set the previous_coverage for the next iteration
#     previous_coverage <- vax_coverage_list[year]
#   }
#   
#   # Return random values between min(vax_coverage_list) and base_vax_cov if mdv_campaign_budget/ target vaccination is 0
#   if(max(vax_coverage_list) < base_vax_cov){
#     return(runif(n = horizon, min = min(vax_coverage_list), max = base_vax_cov*1.2))
#   } else {
#     return(vax_coverage_list)
#   }
# }
# 



# Initialize. Handles the workaround for horizon being 1
initialize <- function(horizon) {
  only_a_single_year <- 'no'
  if (horizon == 1) {
    horizon <- 2
    only_a_single_year <- 'yes'
  }
  return(only_a_single_year)
}

# Estimate dog population
estimate_dog_population <- function(N, pop, HDR, horizon) {
  HDR <- runif(n = N, min = HDR[1], max = HDR[2])
  dog_pop <- matrix(NA, nrow = N, ncol = horizon)
  for (year in 1:horizon) {
    dog_pop[, year] <- pop / HDR
  }
  return(dog_pop)
}


# Calculate vaccination coverage based on budget or target coverage
calc_vax_coverage <- function(base_vax_cov, target_vax_cov = NULL, mdv_campaign_budget = NULL, 
                              vaccinate_dog_cost = NULL, dog_pop = NULL, horizon, discount = 0) {
  
  # Input validation
  if (is.null(target_vax_cov) && is.null(mdv_campaign_budget)) {
    stop("At least one of 'target_vax_cov' or 'mdv_campaign_budget' must be provided.")
  }
  
  # Use mean cost if a vector is provided
  cost_per_dog <- if (length(vaccinate_dog_cost) > 1) mean(vaccinate_dog_cost) else vaccinate_dog_cost
  
  # Cap target at 0.8 if provided
  if (!is.null(target_vax_cov)) {
    target_vax_cov <- min(target_vax_cov, 0.8)
  }
  
  # Helper: generate non-linear ramp-up to final coverage
  generate_vax_coverage <- function(final_coverage) {
    vax_coverage_list <- numeric(horizon)
    previous_coverage <- base_vax_cov
    for (year in 1:horizon) {
      if (year <= 3) {
        vax_coverage_list[year] <- previous_coverage + (final_coverage - previous_coverage) * (year / 3)
      } else {
        vax_coverage_list[year] <- final_coverage
      }
      previous_coverage <- vax_coverage_list[year]
    }
    vax_coverage_list <- c(base_vax_cov, vax_coverage_list[-horizon])
    return(vax_coverage_list)
  }
  
  # Case 1: Only target provided
  if (!is.null(target_vax_cov) && is.null(mdv_campaign_budget)) {
    return(generate_vax_coverage(target_vax_cov))
  }
  
  # Case 2: Only budget provided
  if (!is.null(mdv_campaign_budget) && is.null(target_vax_cov)) {
    if (is.null(cost_per_dog) || is.null(dog_pop)) {
      stop("Both 'vaccinate_dog_cost' and 'dog_pop' must be provided.")
    }
    max_achievable <- min(floor(mdv_campaign_budget / cost_per_dog) / dog_pop, 0.8)
    return(generate_vax_coverage(max_achievable))
  }
  
  # Case 3: Both target and budget provided
  if (!is.null(target_vax_cov) && !is.null(mdv_campaign_budget)) {
    if (is.null(cost_per_dog) || is.null(dog_pop)) {
      stop("Both 'vaccinate_dog_cost' and 'dog_pop' must be provided.")
    }
    max_achievable <- min(floor(mdv_campaign_budget / cost_per_dog) / dog_pop, 0.8)
    
    if (max_achievable >= target_vax_cov) {
      return(generate_vax_coverage(target_vax_cov))
    } else {
      message("Budget is insufficient to achieve target coverage; using budget-based maximum coverage.")
      return(generate_vax_coverage(max_achievable))
    }
  }
}



# Calculate vaccinated and susceptible dog populations
calculate_vaccinated_and_susceptible <- function(N, horizon, dog_pop, vax_cov) {
  ts_dogs_vaccinated <- matrix(NA, nrow = N, ncol = horizon)
  sus_dogs <- matrix(NA, nrow = N, ncol = horizon)
  
  for (year in 1:horizon) {
    ts_dogs_vaccinated[, year] <- dog_pop[, year] * vax_cov[[year]]
    sus_dogs[, year] <- dog_pop[, year] - ts_dogs_vaccinated[, year]
  }
  return(list(ts_dogs_vaccinated = ts_dogs_vaccinated, sus_dogs = sus_dogs))
}



# Calculate MDV campaign costs
calculate_campaign_cost <- function(N, horizon, mdv_campaign_budget, ts_dogs_vaccinated, vaccinate_dog_cost) {
  MDV_campaign_cost <- matrix(NA, nrow = N, ncol = horizon)
  
  if (!is.null(mdv_campaign_budget)) {
    for (year in 1:horizon) {
      MDV_campaign_cost[, year] <- mdv_campaign_budget
    }
  } else {
    for (year in 1:horizon) {
      MDV_campaign_cost[, year] <- ts_dogs_vaccinated[, year] *
        runif(n = N, min = vaccinate_dog_cost[1], max = vaccinate_dog_cost[2])
    }
  }
  return(MDV_campaign_cost)
}

#rabies incidence used to cap the predictions for rabid dogs. Because there has always been vaccination in the Serengeti (which the model is trained on), 
#predictions when vax_cov is zero can exceed realistic values

predict_cases <- function(nreps=N, vax_cov, horizon, dog_pop,
                          vax_model_path = "./data/cases_from_vax_model.rds", 
                          vax_case_model_path = "./data/cases_from_vax+cases_model.rds",
                          rabies_inc= rabies_inc){
  set.seed(123)
  
  # Load models and extract parameter samples
  vax_model <- readRDS(vax_model_path)
  vax_case_model <- readRDS(vax_case_model_path)
  vax_model_samples <- posterior_samples(vax_model)[,1:3]
  vax_case_model_samples <- posterior_samples(vax_case_model)[,1:4]
  
  # Set up model inputs for a district
  incidence_adjust <- 0.0001394842
  
  # Initialize output matrix
  cases_mat <- matrix(NA, nrow = nreps, ncol = horizon)
  vc_last_year <- vax_cov
  
  # Estimate cases 
  for(rep in 1:nreps){
    pars_sim <- vax_model_samples[sample.int(nrow(vax_model_samples), size=1), ]
    mu <- exp(sum(pars_sim[1:2]*c(1,vc_last_year[1]),log(dog_pop[rep])))
    cases_mat[rep,1] <- min(rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[3])), rabies_inc[2] * dog_pop[rep])
    pars_sim <- vax_case_model_samples[sample.int(nrow(vax_case_model_samples),size=1), ]
    
    for(year in 2:horizon){
      mu <- exp(sum(pars_sim[1:3]*c(1,vc_last_year[year],log(cases_mat[rep, year-1]/dog_pop[rep]+incidence_adjust)),log(dog_pop[rep])))
      cases_mat[rep,year] <- min(rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[4])), rabies_inc[2] * dog_pop[rep])
    }  
  }
  
  return(cases_mat)
}



# Simulate expected exposures for a given number of rabid dogs (
nBitesBiters <- function(dog_pop, pBite, pBiteK){
  bites_by_dog <- rnbinom(n = dog_pop,  mu = pBite, size = pBiteK)
  nBites=sum(bites_by_dog)              # bites
  nBiters=length(which(bites_by_dog>0)) # number of biting dogs
  # Return as a named list
  return(list(nBites = nBites, nBiters = nBiters, bites_by_dog = (bites_by_dog[bites_by_dog>0])))
}


# Calculate rabid dogs and human exposures
predict_rabies <- function(N, horizon, vax_cov, dog_pop, rabies_inc, mu, k) {
  ts_rabid_dogs <- predict_cases(nreps = N, vax_cov = vax_cov, horizon = horizon, dog_pop = dog_pop, 
                                 rabies_inc = rabies_inc)
  ts_exposures <- matrix(NA, nrow = N, ncol = horizon)
  ts_rabid_biting_dogs <- matrix(NA, nrow = N, ncol = horizon)
  
  for (year in seq(1, horizon)) {
    output <- sapply(FUN = nBitesBiters, pBite = mu, pBiteK = k, X = ts_rabid_dogs[, year])
    ts_exposures[, year] <- unlist(output[1, ])
    ts_rabid_biting_dogs[, year] <- unlist(output[2, ])
  }
  return(list(ts_rabid_dogs = ts_rabid_dogs, ts_exposures = ts_exposures, ts_rabid_biting_dogs = ts_rabid_biting_dogs))
}


# Healthy bites
calculate_healthy_bites <- function(N, horizon, dog_pop, pBite_healthy){
  ts_healthy_bites <- matrix(nrow = N, ncol = horizon)
  for (year in seq(1, horizon)){
    ts_healthy_bites[,year] <- rbinom(n=N,  size = round(dog_pop), prob = pBite_healthy)
  }
  return(ts_healthy_bites)
}


# Investigate rabid biting dogs and healthy bites (IBCM)
investigate_bites <- function(N, horizon, ts_rabid_biting_dogs, pInvestigate, pFound, pTestable #, ts_healthy_bites, pFP
) {
  # Rabid dog investigations
  ts_rabid_biting_investigated <- matrix(NA, nrow = N, ncol = horizon)
  ts_rabid_biting_found <- matrix(NA, nrow = N, ncol = horizon)
  ts_rabid_biting_testable <- matrix(NA, nrow = N, ncol = horizon)
  
  for (year in 1:horizon) {
    ts_rabid_biting_investigated[, year] <- rbinom(n = N, size = ts_rabid_biting_dogs[, year], prob = pInvestigate)
    ts_rabid_biting_found[, year] <- rbinom(n = N, size = ts_rabid_biting_investigated[, year], prob = pFound)
    ts_rabid_biting_testable[, year] <- rbinom(n = N, size = ts_rabid_biting_found[, year], prob = pTestable)
  }
  
  return(list(
    ts_rabid_biting_investigated = ts_rabid_biting_investigated,
    ts_rabid_biting_found = ts_rabid_biting_found,
    ts_rabid_biting_testable = ts_rabid_biting_testable
  ))
}




# healthcare-seeking behavior
calculate_healthcare <- function(N, horizon, ts_exposures, ts_healthy_bites, pSeek_exposure, pStart_exposure, pComplete_exposure,
                                 pSeek_healthy, pStart_healthy, pComplete_healthy) {
  ts_exposures_seek_care <- matrix(nrow = N, ncol = horizon)
  ts_exp_start <- matrix(nrow = N, ncol = horizon)
  ts_exp_complete <- matrix(nrow = N, ncol = horizon)
  
  ts_healthy_seek_care <- matrix(nrow = N, ncol = horizon)
  ts_healthy_start <- matrix(nrow = N, ncol = horizon)
  ts_healthy_complete <- matrix(nrow = N, ncol = horizon)
  
  
  
  for (year in seq(1, horizon)) {
    ts_exposures_seek_care[, year] <- rbinom(n = N, size = ts_exposures[, year], prob = pSeek_exposure)
    ts_exp_start[,year] <- rbinom(n=N,  size = ts_exposures_seek_care[,year], prob = pStart_exposure)
    ts_exp_complete[,year] <- rbinom(n=N,  size = ts_exp_start[,year], prob = pComplete_exposure)
    
    ts_healthy_seek_care[, year] <- rbinom(n = N, size = ts_healthy_bites[, year], prob = pSeek_healthy)
    ts_healthy_start[,year] <- rbinom(n=N,  size = ts_healthy_seek_care[,year], prob = pStart_healthy)
    ts_healthy_complete[,year] <- rbinom(n=N,  size = ts_healthy_start[,year], prob = pComplete_healthy)
    
  }
  
  # ts_exp_do_not_seek_care <- ts_exposures - ts_exposures_seek_care
  # ts_exp_no_start <- (ts_exposures_seek_care - ts_exp_start) + ts_exp_do_not_seek_care
  # ts_exp_incomplete <- ts_exp_start - ts_exp_complete
  
  # Total bite victims seeking care
  # total_seek_care = ts_exposures_seek_care + ts_healthy_seek_care
  #ts_total_seek_care_inc = (total_seek_care/pop)*1E5
  
  #ts_exp_seek_care_inc = (ts_exposures_seek_care/pop)*1E5        # per 100k
  #ts_healthy_seek_care_inc = (ts_healthy_seek_care/pop)*1E5
  
  
  # # heathy bites healthcare
  # ts_healthy_do_not_seek_care <- ts_healthy_bites - ts_healthy_seek_care
  # ts_healthy_no_start <- (ts_healthy_seek_care - ts_healthy_start) + ts_healthy_do_not_seek_care
  # ts_healthy_incomplete <- ts_healthy_start - ts_healthy_complete
  
  return(list(
    ts_exp_seek_care = ts_exposures_seek_care,
    #ts_exp_do_not_seek_care = ts_exp_do_not_seek_care,
    ts_exp_start = ts_exp_start,
    #ts_exp_no_start = ts_exp_no_start,
    ts_exp_complete = ts_exp_complete,
    #ts_exp_incomplete = ts_exp_incomplete,
    
    
    ts_healthy_seek_care = ts_healthy_seek_care,
    #ts_healthy_do_not_seek_care = ts_healthy_do_not_seek_care,
    ts_healthy_start = ts_healthy_start,
    #ts_healthy_no_start = ts_healthy_no_start,
    ts_healthy_complete = ts_healthy_complete
    #ts_healthy_incomplete = ts_healthy_incomplete,
    
    #total_seek_care = total_seek_care,
    #ts_healthy_seek_care_inc = ts_healthy_seek_care_inc
  ))
}


# Predict deaths based on healthcare seeking
predict_deaths_and_deathsaverted_PEP <- function(N, horizon, ts_exposures, ts_exp_no_start, ts_exp_complete, ts_exp_incomplete, pDeath, pPrevent) {
  
  ts_deaths_no_PEP <- matrix(nrow = N, ncol = horizon)
  deaths_incomplete_PEP <- matrix(nrow = N, ncol = horizon)
  
  deaths_averted_PEP_complete <- matrix(nrow = N, ncol = horizon)
  deaths_averted_PEP_incomplete <- matrix(nrow = N, ncol = horizon)
  
  
  for (year in seq(1, horizon)) {
    ts_deaths_no_PEP[, year] <- rbinom(n = N, size = ts_exp_no_start[, year], prob = pDeath)
    deaths_incomplete_PEP[, year] <- rbinom(n = N, size = ts_exp_incomplete[, year], prob = (1 - pPrevent))
    # deaths averted 
    deaths_averted_PEP_complete[,year] <- rbinom(n = N, size = ts_exp_complete[, year], prob = pPrevent)
    deaths_averted_PEP_incomplete[, year] <- rbinom(n = N, size = (ts_exp_incomplete[, year] - deaths_incomplete_PEP[, year]), prob = (pPrevent * pDeath))
    
  }
  
  ts_deaths <- ts_deaths_no_PEP + deaths_incomplete_PEP
  ts_deaths_averted_PEP <- deaths_averted_PEP_complete + deaths_averted_PEP_incomplete
  
  return(list(ts_deaths = ts_deaths,
              ts_deaths_averted_PEP = ts_deaths_averted_PEP)
  )
}





# Deaths averted MDV
compute_deaths_averted_MDV <- function(N, horizon, mdv_campaign_budget, target_vax_cov, base_vax_cov, 
                                       dog_pop, rabies_inc, mu, k, pDeath, ts_exposures) {
  
  # Initialize outputs
  ts_deaths_averted_MDV <- NULL
  ts_deaths_no_MDV <- NULL
  
  # Run only if MDV budget is non-zero or if target coverage is greater than base coverage
  if ((!is.null(mdv_campaign_budget) && mdv_campaign_budget != 0) || 
      (target_vax_cov > base_vax_cov)) {
    
    vax_cov_no_MDV <- calc_vax_coverage(base_vax_cov, target_vax_cov = 0, dog_pop = dog_pop,
                                        horizon = horizon) # No MDV efforts
    
    # Predict rabid dogs without MDV
    ts_rabid_dogs_no_MDV <- predict_cases(
      nreps = N, 
      vax_cov = vax_cov_no_MDV, 
      horizon = horizon, 
      dog_pop = dog_pop, 
      rabies_inc = rabies_inc
    )
    
    # Predict human exposures without MDV
    ts_exposures_no_MDV <- matrix(NA, nrow = N, ncol = horizon)
    for (year in seq(1, horizon)) {
      output_no_MDV <- sapply(FUN = nBitesBiters, pBite = mu, pBiteK = k, X = ts_rabid_dogs_no_MDV[, year]) 
      ts_exposures_no_MDV[, year] <- unlist(output_no_MDV[1,])  # Extract nBites
    }
    
    # Exposures averted due to dog
    ts_exposures_avoided <- pmax((ts_exposures_no_MDV - ts_exposures),0)
    
    # Deaths averted by MDV
    ts_deaths_averted_MDV <- matrix(nrow = N, ncol = horizon)
    for (year in seq(1, horizon)) {
      ts_deaths_averted_MDV[, year] <- rbinom(n = N, size = ts_exposures_avoided[, year], prob = pDeath)
    }
    
  } else {
    ts_deaths_averted_MDV <- matrix(0, nrow = N, ncol = horizon)
  }
  
  return(ts_deaths_averted_MDV)
}


summarise_stochasticity <- function(mat=status_quo$ts_deaths_averted_PEP, scenario="status quo") {
  # Calculate summary statistics per column
  result <- apply(mat, 2, function(x) {
    c(
      LL = quantile(x, 0.025, na.rm = TRUE),
      Median = median(x, na.rm = TRUE),
      UL = quantile(x, 0.975, na.rm = TRUE)
    )
  })
  
  # Convert result to a data frame
  result_df <- as.data.frame(t(result)) %>%
    dplyr::rename(
      LL = `LL.2.5%`,
      UL = `UL.97.5%`
    )
  
  # Add 'year' and 'scenario' columns
  result_df$year <- seq_len(ncol(mat))
  result_df$scenario <- scenario
  
  return(result_df)
}







horizon_CEA <- function(baseline_sum, comparator_sum){
  comparator_sum$deaths_avert <- baseline_sum$deaths - comparator_sum$deaths
  comparator_sum$cost_per_deaths_avert <- comparator_sum$cost/comparator_sum$deaths_avert
  return(comparator_sum)
}


# # Function to calculate the incremental cost-effectiveness comparing 2 scenarios (baseline vs intervention)
# calc_ICER <- function(baseline, comparator){ # provide scenario summaries
#   ICER_summary <- data.frame(
#     cost_diff = baseline$cost - comparator$cost,
#     death_diff = baseline$deaths - comparator$deaths
#   )
#   ICER_summary$baseline = paste0(baseline$scenario[1], "-", baseline$discount[1], "-", baseline$PEP)
#   ICER_summary$comparator = paste0(comparator$scenario[1], "-", comparator$discount[1], "-", comparator$PEP)
#   ICER_summary$CIs = baseline$CIs
#   return(ICER_summary)
# }

