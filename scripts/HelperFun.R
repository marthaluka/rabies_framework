


# Vaccination coverage attained given a certain target coverage
vax_coverage_over_x_years <- function(base_vax_cov, target_vax_cov, horizon){
  # # Check inputs
  # if(!is.numeric(base_vax_cov) || base_vax_cov < 0 || base_vax_cov > 1) stop("Invalid initial vaccination coverage.")
  # if(!is.numeric(target_vax_cov) || target_vax_cov < 0 || target_vax_cov > 1) stop("Invalid target vaccination coverage.")
  # if(!is.numeric(horizon) || horizon <= 0) stop("Horizon must be a positive number.")
  
  # Linear interpolation between initial and target vaccination coverage over 3 years
      # # assume it takes 3 years to hit target vaccination coverage
  interpolated_values <- map2(base_vax_cov, target_vax_cov, seq, length.out = 3)
  
  # If horizon is set to less than 3 years
  if (horizon < 3){
    annual_vax_cov = interpolated_values[[1]][c(1:horizon)]
  } else {
    sustained_values <- rep(target_vax_cov, (horizon - 3))
    annual_vax_cov <- unlist(append(interpolated_values, sustained_values))
  }
  
  # Return random values between target and base_vax_cov if target is 0
  if(target_vax_cov == 0){
    return(runif(n = horizon, min = target_vax_cov, max = base_vax_cov*1.2))
  } else {
    return(annual_vax_cov)
  }
}



# modifying this to take set budget as input- in place of target coverage 
vax_coverage_from_budget <- function(campaign_budget, base_vax_cov, vaccinate_dog_cost, dog_pop, horizon, discount){
  
  vax_coverage_list <- numeric(horizon) # initialize an empty numeric vector of length 'horizon'
  
  previous_coverage <- base_vax_cov
  for(year in 1:horizon){
    # Calculate the discounted budget for the current year
    discounted_budget <- campaign_budget * (1 - discount)^(year - 1)
    
    # Calculate number of dogs that can be vaccinated with the discounted budget
    dogs_vaccinated <- floor(discounted_budget / vaccinate_dog_cost)
    
    # Calculate the maximum potential coverage based on the discounted budget
    potential_coverage <- min(dogs_vaccinated / dog_pop, 0.8)
    
    # Gradually increase the coverage over three years
    if (year <= 3) {
      vax_coverage_list[year] <- previous_coverage + (potential_coverage - previous_coverage) * (year/3)
    } else {
      vax_coverage_list[year] <- potential_coverage
    }
    
    # Set the previous_coverage for the next iteration
    previous_coverage <- vax_coverage_list[year]
  }
  
  # Return random values between min(vax_coverage_list) and base_vax_cov if campaign_budget/ target vaccination is 0
  if(max(vax_coverage_list) < base_vax_cov){
    return(runif(n = horizon, min = min(vax_coverage_list), max = base_vax_cov*1.2))
  } else {
    return(vax_coverage_list)
  }
}

#rabies incidence used to cap the predictions for rabid dogs. Because there has always been vaccination in the Serengeti (which the model is trained on), 
  #predictions when vax_cov is zero can exceed max possible values
rabies_inc= c(0.0075,0.0125)

predict_cases <- function(nreps=N, vax_cov, horizon, dog_pop, rabies_inc, 
                          vax_model_path = "./data/cases_from_vax_par_samples.csv", 
                          vax_case_model_path = "./data/cases_from_vax+cases_par_samples.csv"){
  set.seed(123)
  
  # Load model inputs from provided paths
  vax_model_samples <- read.csv(vax_model_path)
  vax_case_model_samples <- read.csv(vax_case_model_path)
  
  # Initialize output matrix
  cases_mat <- matrix(NA, nrow = nreps, ncol = horizon)
  #vc_last_year <- vax_coverage_over_x_years(base_vax_cov, target_vax_cov, horizon)
  vc_last_year <- vax_cov
  
  # Estimate cases 
  for(rep in 1:nreps){
    pars_sim <- vax_model_samples[sample.int(nrow(vax_model_samples), size = 1), ]
    mu <- exp(sum(pars_sim[1:2] * c(1, vc_last_year[1]), log(dog_pop[rep])))
    cases_mat[rep, 1] <- min(rnbinom(n = 1, mu = mu, size = as.numeric(pars_sim[3])), rabies_inc[2] * dog_pop[rep])
    
    pars_sim <- vax_case_model_samples[sample.int(nrow(vax_case_model_samples), size = 1), ]
    
    for(year in 2:horizon){
      mu <- exp(sum(pars_sim[1:3] * c(1, vc_last_year[year], log(cases_mat[rep, year - 1] + 1)), log(dog_pop[rep])))
      cases_mat[rep, year] <- min(rnbinom(n = 1, mu = mu, size = as.numeric(pars_sim[4])), rabies_inc[2] * dog_pop[rep])
    }
  }
  return(cases_mat)
}


# dog_pop <- matrix(runif(N, 1000, 9877),nrow=N,ncol=horizon)
# 
# predict_cases(vax_cov = vax_cov, horizon =horizon, dog_pop= dog_pop, rabies_inc=c(0.0075, 0.0125))



# Rabid bites
# Simulate expected exposures for a given number of rabid dogs (
nBites <- function(dog_pop, pBite, pBiteK){
  bites_by_dog <- rnbinom(n = dog_pop,  mu = pBite, size = pBiteK)
  nBites=sum(bites_by_dog)
  return(nBites)
}

# number of biting dogs
nBiters <- function(dog_pop, pBite, pBiteK){
  bites_by_dog <- rnbinom(n = dog_pop,  mu = pBite, size = pBiteK)
  nBiters=length(which(bites_by_dog>0))
  return(nBiters)
}


horizon_CEA <- function(baseline_sum, comparator_sum){
  comparator_sum$deaths_avert <- baseline_sum$deaths - comparator_sum$deaths
  comparator_sum$cost_per_deaths_avert <- comparator_sum$cost/comparator_sum$deaths_avert
  return(comparator_sum)
}


# # Function to calculate the incremental cost-effectiveness comparing 2 scenarios (baseline vs intervention)
# calc_ICER <- function(baseline, comparator, discount){ # provide scenario summaries
#   ICER_summary <- data.frame(
#     cost_diff = baseline$cost - comparator$cost,
#     death_diff = baseline$deaths - comparator$deaths
#   )
#   ICER_summary$baseline = paste0(baseline$scenario[1], "-", baseline$discount[1], "-", baseline$PEP)
#   ICER_summary$comparator = paste0(comparator$scenario[1], "-", comparator$discount[1], "-", comparator$PEP)
#   ICER_summary$CIs = baseline$CIs
#   return(ICER_summary)
# }

