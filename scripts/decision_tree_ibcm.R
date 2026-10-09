


# model with IBCM



require(pacman)
pacman::p_load(tidyverse,   # cleaning, wrangling
               brms,      # models
               rlang,
               lubridate, # dates
               matrixStats,
               scales,
               readxl,
               ggrepel,
               cowplot,
               patchwork,
               purrr,
               furrr,
               future
)


# source helper functions  
source("./scripts/HelperFun.R")
source("./scripts/ibcm_helper.R")

decision_tree_ibcm <- function(N = 10, pop = 35e6, HDR = c(16,17), unowned_prop = 0, horizon = 5, 
                               discount = 0.03, mu = 0.38, k = 0.72, bpi = 1530, pStart_healthy = 0.9684211,
                               pCompliance_healthy = 0.9654, pSeek_exposure = 0.9, pStart_exposure = 0.9684211,
                               pCompliance_exp = 0.9654, pDeath = 0.17,pPrevent_complete = 0.999, 
                               pPrevent_incomplete = 0.986, rabies_inc = c(0.0075, 0.0125), mdv_unowned_budget = NULL, 
                               mdv_owned_budget = NULL,vaccinate_owned_dog_cost = c(0.5, 1), 
                               vaccinate_unowned_dog_cost = c(3.5, 4.5), base_vax_cov_owned = 0.5,   
                               target_vax_cov_owned = 0.5, base_vax_cov_unowned = 0.03, target_vax_cov_unowned = 0.03,
                               years_to_target = 3,  RIG_cov = 0.38, PEP_vials_per_pt = 0.66,
                               human_vaccine_cost_per_vial = 5, RIG_cost = 12, seed = 123, dog_burnin = 1,
                               # IBCM
                               pInvestigate = 0.5, pFound = 0.4, pTestable = 0.2, pFS = 0.05, 
                               dogs_per_reactive_vax = 20, max_pSeek_exposure = 0.95,
                               max_pCompliance_exp = 0.99, #min_pCompliance_healthy = 0.05,
                               max_dog_vax_cov = 0.8, max_reactive_vaccinations = 360,
                               reactive_vax_cost_per_dog = 4,
                               investigation_cost_per_dog = 0, dog_test_cost_per_dog = 0,
                               ibcm_increase_exposure_care_seeking = TRUE, ibcm_increase_exposure_compliance = TRUE,
                               ibcm_reactive_vaccination = TRUE #,ibcm_decrease_healthy_compliance = TRUE 
                               ) {
  
  # The two new unit-cost defaults are zero because they are setting-specific.
  # investigation_cost_per_dog should include the full cost of investigating
  # one biting dog, including quarantine/observation where applicable.
  ibcm_switches <- c(
    ibcm_increase_exposure_care_seeking,
    ibcm_increase_exposure_compliance,
    #ibcm_decrease_healthy_compliance,
    ibcm_reactive_vaccination
  )
  if (any(lengths(list(
    ibcm_increase_exposure_care_seeking,
    ibcm_increase_exposure_compliance,
    #ibcm_decrease_healthy_compliance,
    ibcm_reactive_vaccination
  )) != 1L) ||
  any(is.na(ibcm_switches)) ||
  !all(ibcm_switches %in% c(TRUE, FALSE))) {
    stop("All IBCM benefit switches must be a single TRUE or FALSE.")
  }
  
  
  total_horizon <- horizon + dog_burnin
  
  set.seed(seed)
  # ---------------------------------------------------------------------------#
  # 1: Dog population & vaccination   #######
  # ---------------------------------------------------------------------------#
  dog_pop      <- estimate_dog_population(N, pop, HDR, total_horizon)
  unowned_dogs <- matrix(rbinom(N * total_horizon, as.integer(dog_pop), unowned_prop),
                         nrow = N, ncol = total_horizon)
  owned_dogs   <- dog_pop - unowned_dogs
  owned_prop   <- 1 - unowned_prop
  
  vax_cov_owned <- calc_vax_coverage(
    base_vax_cov = base_vax_cov_owned, target_vax_cov = target_vax_cov_owned,
    mdv_campaign_budget = mdv_owned_budget, vaccinate_dog_cost = vaccinate_owned_dog_cost,
    dog_pop = owned_dogs, horizon = total_horizon, discount = discount,
    years_to_target = years_to_target, dog_burnin = dog_burnin)
  
  vax_cov_unowned <- calc_vax_coverage(
    base_vax_cov = base_vax_cov_unowned, target_vax_cov = target_vax_cov_unowned,
    mdv_campaign_budget = mdv_unowned_budget, vaccinate_dog_cost = vaccinate_unowned_dog_cost,
    dog_pop = unowned_dogs, horizon = total_horizon, discount = discount,
    years_to_target = years_to_target, dog_burnin = dog_burnin)
  
  dog_vax_cov <- (vax_cov_unowned * unowned_prop) + (vax_cov_owned * owned_prop)
  
  # IBCM changes completion probabilities once, at population level. Its
  # care-seeking and reactive-vaccination effects are simulated dynamically.
  
  ibcm_adj <- apply_ibcm_effects(
    ibcm = "yes", #rabies_inc = rabies_inc, mu = mu,
    #pSeek_exposure = pSeek_exposure,
    pCompliance_exp = pCompliance_exp,
    #pCompliance_healthy = pCompliance_healthy,
    #dog_vax_cov = dog_vax_cov, dog_pop = dog_pop,
    pInvestigate = pInvestigate, pFound = pFound, pTestable = pTestable,
    #dogs_per_reactive_vax = dogs_per_reactive_vax,
    #max_dog_vax_cov = max_dog_vax_cov
    #max_pSeek_exposure = max_pSeek_exposure,
    max_pCompliance_exp = max_pCompliance_exp
    # ,
    # min_pCompliance_healthy = min_pCompliance_healthy
  )
  if (ibcm_increase_exposure_compliance) {
    pCompliance_exp <- ibcm_adj$pCompliance_exp
  }
  # if (ibcm_decrease_healthy_compliance) {
  #   pCompliance_healthy <- ibcm_adj$pCompliance_healthy
  # }
  
  
  # continue with dog dynamics
  
  vax_results        <- calculate_vaccinated_and_susceptible(N, total_horizon, dog_pop, dog_vax_cov)
  vaccinated_unowned <- calculate_vaccinated_and_susceptible(N, total_horizon, unowned_dogs, vax_cov_unowned)$ts_dogs_vaccinated
  vaccinated_owned   <- calculate_vaccinated_and_susceptible(N, total_horizon, owned_dogs,   vax_cov_owned)$ts_dogs_vaccinated
  
  MDV_campaign_cost <-
    calculate_campaign_cost(N, total_horizon, mdv_unowned_budget, vaccinated_unowned, vaccinate_unowned_dog_cost) +
    calculate_campaign_cost(N, total_horizon, mdv_owned_budget,   vaccinated_owned,   vaccinate_owned_dog_cost)
  
  
  dynamics_full <- simulate_ibcm_dynamics(
    N = N, total_horizon = total_horizon,
    baseline_vax_cov = dog_vax_cov, dog_pop = dog_pop,
    rabies_inc = rabies_inc, mu = mu, k = k,
    pSeek_exposure = pSeek_exposure, pFS = pFS,
    max_pSeek_exposure = max_pSeek_exposure,
    pInvestigate = pInvestigate, pFound = pFound, pTestable = pTestable,
    dogs_per_reactive_vax = dogs_per_reactive_vax,
    max_reactive_vaccinations = if (ibcm_reactive_vaccination) {
      max_reactive_vaccinations
    } else {
      0
    },
    max_dog_vax_cov = max_dog_vax_cov, seed = seed
  )
  
  
  keep_cols <- (dog_burnin + 1):total_horizon
  strip_burnin <- function(mat) mat[, keep_cols, drop = FALSE]
  dynamic_names <- setdiff(names(dynamics_full), "bites_by_dog")
  dynamic_names <- dynamic_names[vapply(
    dynamics_full[dynamic_names],
    function(x) is.matrix(x) && ncol(x) == total_horizon,
    logical(1)
  )]
  for (nm in dynamic_names) assign(nm, strip_burnin(dynamics_full[[nm]]))
  
  rabies_results <- list(
    ts_rabid_dogs = ts_rabid_dogs,
    ts_exposures = ts_exposures,
    ts_rabid_biting_dogs = ts_rabid_biting_dogs
  )
  MDV_campaign_cost <- strip_burnin(MDV_campaign_cost)
  vax_results <- calculate_vaccinated_and_susceptible(
    N, horizon, strip_burnin(dog_pop), ts_dog_vax_cov_effective
  )
  
  ts_total_bite_presentations <- matrix(
    rbinom(N * horizon, pop, bpi / 100000), N, horizon
  )
  
  # Contact tracing can still be simulated inside the dynamic IBCM process
  # when its care-seeking benefit is disabled. Setting the reached bites to
  # zero here switches off only their addition to human care seeking, leaving
  # investigations and the other independently selected IBCM effects intact.
  if (!ibcm_increase_exposure_care_seeking) {
    ts_bites_reached_by_ibcm[,] <- 0
  }
  
  ts_exp_seek_care <- pmin(
    ts_exp_seek_care_initial + ts_bites_reached_by_ibcm,
    ts_exposures
  )
  ts_exp_do_not_seek_care <- ts_exposures - ts_exp_seek_care
  ts_healthy_seek_care <- pmax(ts_total_bite_presentations - ts_exp_seek_care_initial, 0)
  ts_healthy_biting_investigated <- matrix(
    rbinom(N * horizon, as.vector(ts_healthy_seek_care), pFS), N, horizon
  )
  
  # Update ts_total_bite_presentations as more people have been "fished out" by IBCM
  ts_total_bite_presentations <- ts_total_bite_presentations + ts_bites_reached_by_ibcm
  
  
  ts_dog_investigations <- ts_rabid_biting_investigated + ts_healthy_biting_investigated
  # Assumes 1 healthy dog = 1 bite !!!!
  
  # Healthy dogs progressing through the IBCM investigation pathway
  ts_healthy_biting_found <- matrix(rbinom(N * horizon, as.vector(ts_healthy_biting_investigated), pFound), N, horizon)
  ts_healthy_biting_tested <- matrix(rbinom(N * horizon, as.vector(ts_healthy_biting_found), pTestable), N, horizon)
  
  # Total IBCM workload
  ts_dogs_found <- ts_rabid_biting_found + ts_healthy_biting_found
  ts_dogs_tested <- ts_rabid_biting_tested + ts_healthy_biting_tested
  
  
  ts_reactive_vax_cost <-
    ts_dogs_reactive_vaccinated * reactive_vax_cost_per_dog
  ts_investigation_cost <-
    ts_dog_investigations * investigation_cost_per_dog
  ts_dog_testing_cost <-
    ts_dogs_tested * dog_test_cost_per_dog
  
  # ---------------------------------------------------------------------------#
  ##  3B: Biologicals start & compliance ######
  # ---------------------------------------------------------------------------#
  ## Exposures 
  ts_exp_start       <- matrix(rbinom(N * horizon, as.vector(ts_exp_seek_care), pStart_exposure), N, horizon)
  ts_exp_Seek_no_start   <- ts_exp_seek_care - ts_exp_start
  ts_exp_nostartPEP   <- ts_exp_do_not_seek_care + ts_exp_Seek_no_start
  ts_exp_second_dose <- matrix(rbinom(N * horizon, as.vector(ts_exp_start),      pCompliance_exp), N, horizon)
  ts_exp_complete <- matrix(rbinom(N * horizon, as.vector(ts_exp_second_dose),pCompliance_exp), N, horizon)
  ts_exp_incomplete <- ts_exp_start - ts_exp_complete
  
  ## Healthy bites 
  ts_healthy_start       <- matrix(rbinom(N * horizon, as.vector(ts_healthy_seek_care),   pStart_healthy),    N, horizon)
  ts_healthy_second_dose <- matrix(rbinom(N * horizon, as.vector(ts_healthy_start),       pCompliance_healthy),N, horizon)
  ts_healthy_complete    <- matrix(rbinom(N * horizon, as.vector(ts_healthy_second_dose), pCompliance_healthy),N, horizon)
  ts_healthy_incomplete  <- ts_healthy_start - ts_healthy_complete
  
  
  ## RIG
  # With IBCM, RIG administered to true exposures (+ misclassed low risk)
  RIG_eligible <- ts_exp_start + ts_healthy_biting_investigated
  ts_RIG <- matrix(
    rbinom(N * horizon, as.vector(RIG_eligible), RIG_cov), N, horizon
  )
  
  # ---------------------------------------------------------------------------#
  # 5: Outcomes  ########
  ## 5A: deaths ########
  # ---------------------------------------------------------------------------#
  
  ts_deaths_no_PEP <- matrix(
    rbinom(N * horizon, as.vector(ts_exp_nostartPEP), pDeath), N, horizon)
  
  deaths_incomplete_PEP <- matrix(
    rbinom(N * horizon, as.vector(ts_exp_incomplete), ((1 - pPrevent_incomplete)* pDeath)), N, horizon)
  deaths_complete_PEP   <- matrix(
    rbinom(N * horizon, as.vector(ts_exp_complete),   ((1 - pPrevent_complete)* pDeath)),   N, horizon)
  
  ts_deaths <- ts_deaths_no_PEP + deaths_incomplete_PEP + deaths_complete_PEP
  
  ## 5B: Deaths averted  ########
  ### PEP
  deaths_averted_PEP_complete   <- matrix(
    rbinom(N * horizon, as.vector(ts_exp_complete),   pPrevent_complete   * pDeath), N, horizon)
  deaths_averted_PEP_incomplete <- matrix(
    rbinom(N * horizon, as.vector(ts_exp_incomplete), pPrevent_incomplete * pDeath), N, horizon)
  
  ts_deaths_averted_PEP    <- deaths_averted_PEP_complete + deaths_averted_PEP_incomplete
  
  ### IBCM ??? --- No because it works by either increasing MDV or PEP uptake....
  
  
  # ---------------------------------------------------------------------------#
  # MDV counterfactual — ### OPT: reuse dog_pop; only recompute what changes
  # ---------------------------------------------------------------------------#
  
  vax_cov_no_MDV <-
    calc_vax_coverage(0.05, 0.05, NULL, vaccinate_owned_dog_cost, owned_dogs,
                      total_horizon, discount, years_to_target, dog_burnin) * owned_prop +
    calc_vax_coverage(0,    0,    NULL, vaccinate_unowned_dog_cost, unowned_dogs,
                      total_horizon, discount, years_to_target, dog_burnin) * unowned_prop
  
  exposures_no_MDV <- strip_burnin(
    predict_dograbies_split(
      N = N, horizon = total_horizon, vax_cov = vax_cov_no_MDV, dog_pop = dog_pop,
      rabies_inc = rabies_inc, mu = mu, k = k, seed = seed,
      pop_serengeti = 1e5, split_by = "mean")$ts_exposures)
  
  ts_deaths_averted_MDV  <- (exposures_no_MDV - rabies_results$ts_exposures) * pDeath # deterministic to reduce MCMC noise (may lead to NAs)
  # ts_deaths_averted_MDV2 <- matrix(
  #   rbinom(N * horizon, pmax(0L, as.vector(exposures_no_MDV - rabies_results$ts_exposures)), pDeath),
  #   N, horizon)
  
  expected_deaths_no_intervention <- matrix(
    rbinom(N * horizon, as.vector(exposures_no_MDV), pDeath), N, horizon)
  
  ts_deaths_averted  <- expected_deaths_no_intervention - ts_deaths
  
  # # discount deaths averted/ lives saved -- not discounting deaths averted but will discount QALYs gained outside main function
  # disc <- (1 + discount)^(-(0:(horizon - 1)))
  # ts_deaths_averted_PEP   <- sweep(ts_deaths_averted_PEP, 2, disc, `*`)
  # ts_deaths_averted_MDV   <- sweep(ts_deaths_averted_MDV, 2, disc, `*`)
  # ts_deaths_averted       <- sweep(ts_deaths_averted, 2, disc, `*`)
  # ts_deaths_averted2 <- ts_deaths_averted_PEP + ts_deaths_averted_MDV
  
  
  # ---------------------------------------------------------------------------#
  #  6: Economics  ########
  # ---------------------------------------------------------------------------#
  # ts_complete_PEP   <- ts_exp_complete  + ts_healthy_complete
  # ts_incomplete_PEP <- ts_exp_incomplete + ts_healthy_incomplete
  
  ts_PEP_vials    <- (ts_exp_start + ts_healthy_start) * PEP_vials_per_pt
  
  
  # discount costs
  disc <- (1 + discount)^(-(0:(horizon - 1)))
  ts_RIG_cost_per_year <- sweep(ts_RIG        * RIG_cost,                    2, disc, `*`)
  ts_cost_PEP_per_year <- sweep(ts_PEP_vials  * human_vaccine_cost_per_vial, 2, disc, `*`)
  ts_MDV_campaign_cost <- sweep(MDV_campaign_cost,                           2, disc, `*`)
  ts_reactive_vax_cost <- sweep(ts_reactive_vax_cost, 2, disc, `*`)
  ts_investigation_cost <- sweep(ts_investigation_cost, 2, disc, `*`)
  ts_dog_testing_cost <- sweep(ts_dog_testing_cost, 2, disc, `*`)
  
  # All IBCM-specific costs, discounted to the base year.
  ts_ibcm_costs <- ts_reactive_vax_cost +
    ts_investigation_cost +
    ts_dog_testing_cost
  
  ts_cost_per_year <- ts_MDV_campaign_cost + ts_ibcm_costs +
    ts_cost_PEP_per_year + ts_RIG_cost_per_year
  
  # ---------------------------------------------------------------------------#
  # Collate & return  #######
  # ---------------------------------------------------------------------------#
  
  # All `ts_` objects from the local environment
  my_list      <- ls(pattern = "^ts_")
  out_matrices <- mget(my_list)
  
  # ts_ elements from rabies_results (already stripped)
  rabies_ts <- rabies_results[grep("^ts_", names(rabies_results))]
  dogs_ts   <- vax_results[grep("^ts_", names(vax_results))]
  
  # Avoid duplicate names when recursively generated rabies outputs are
  # already present in the local environment.
  repeated_names <- union(names(rabies_ts), names(dogs_ts))
  out_matrices <- c(
    out_matrices[setdiff(names(out_matrices), repeated_names)],
    rabies_ts,
    dogs_ts
  )
  
  return(out_matrices)
}
