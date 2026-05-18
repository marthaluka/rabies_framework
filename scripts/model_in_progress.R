

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
               furrr
)



# Decision tree model that can be applied to create different scenarios 

# source helper functions  
source("./scripts/HelperFun.R")


decision_tree <- function(N = 10, pop = 35e6, HDR = c(16,17), unowned_prop = 0.633, horizon = 5, 
                          discount = 0.03, mu = 0.38, k = 0.72, bpi = 15.3, pStart_healthy = 0.9684211,
                          pCompliance_healthy = 0.9654, pSeek_exposure = 0.9, pStart_exposure = 0.9684211,
                          pCompliance_exp = 0.9654, pDeath = 0.17,pPrevent_complete = 0.999, 
                          pPrevent_incomplete = 0.986, rabies_inc = c(0.0075, 0.0125), mdv_unowned_budget = NULL, 
                          mdv_owned_budget = NULL,vaccinate_owned_dog_cost = c(0.5, 1), 
                          vaccinate_unowned_dog_cost = c(3.5, 4.5), base_vax_cov_owned = 0.5,   
                          target_vax_cov_owned = 0.5, base_vax_cov_unowned = 0.03, target_vax_cov_unowned = 0.03,
                          years_to_target = 3, pInvestigate = 0.5, pFound = 0.4, pTestable = 0.2,
                          pFS = 0.05, RIG_cov = 0.38, PEP_vials_per_pt = 0.66,
                          human_vaccine_cost_per_vial = 5, RIG_cost = 12, seed = 123, 
                          ibcm = "no", dog_burnin = 1) {
  
  total_horizon <- horizon + dog_burnin
  
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
  
  vax_results        <- calculate_vaccinated_and_susceptible(N, total_horizon, dog_pop, dog_vax_cov)
  vaccinated_unowned <- calculate_vaccinated_and_susceptible(N, total_horizon, unowned_dogs, vax_cov_unowned)$ts_dogs_vaccinated
  vaccinated_owned   <- calculate_vaccinated_and_susceptible(N, total_horizon, owned_dogs,   vax_cov_owned)$ts_dogs_vaccinated
  
  MDV_campaign_cost <-
    calculate_campaign_cost(N, total_horizon, mdv_unowned_budget, vaccinated_unowned, vaccinate_unowned_dog_cost) +
    calculate_campaign_cost(N, total_horizon, mdv_owned_budget,   vaccinated_owned,   vaccinate_owned_dog_cost)
  
  # ---------------------------------------------------------------------------#
  # 2: Dogs and human bites: exposures & healthy
  ## 2A: Dog rabies & human exposures — strip burn-in immediately   #######
  # ---------------------------------------------------------------------------#
  rabies_results_full <- predict_dograbies_split(
    N = N, horizon = total_horizon, vax_cov = dog_vax_cov, dog_pop = dog_pop,
    rabies_inc = rabies_inc, mu = mu, k = k, seed = seed,
    pop_serengeti = 1e5, split_by = "mean")
  
  keep_cols   <- (dog_burnin + 1):total_horizon
  strip_burnin <- function(mat) mat[, keep_cols, drop = FALSE]
  
  # ### OPT: bites_by_dog flat-list stripping (column-major: year = ceiling(idx/N))
  bites_keep_idx <- which(ceiling(seq_along(rabies_results_full$bites_by_dog) / N) > dog_burnin)
  
  rabies_results <- list(
    ts_rabid_dogs        = strip_burnin(rabies_results_full$ts_rabid_dogs),
    ts_exposures         = strip_burnin(rabies_results_full$ts_exposures),
    ts_rabid_biting_dogs = strip_burnin(rabies_results_full$ts_rabid_biting_dogs),
    bites_by_dog         = rabies_results_full$bites_by_dog[bites_keep_idx])
  
  vax_results$ts_dogs_vaccinated <- strip_burnin(vax_results$ts_dogs_vaccinated)
  vax_results$sus_dogs           <- strip_burnin(vax_results$sus_dogs)
  MDV_campaign_cost              <- strip_burnin(MDV_campaign_cost)
  
  # ---------------------------------------------------------------------------#
  ##  2B: Healthy bites (human horizon only)   #######
  # ---------------------------------------------------------------------------#
  ts_recorded_bites <- matrix(rbinom(N * horizon, pop, bpi / 1000), N, horizon)
  
  # ---------------------------------------------------------------------------#
  #  3: Hospital ######
  ##  3A: Healthcare seeking #######
  # ---------------------------------------------------------------------------#
  #ts_exp  <- rabies_results$ts_exposures

  ts_exp_seek_care   <- matrix(rbinom(N * horizon, as.vector(rabies_results$ts_exposures),   pSeek_exposure), N, horizon)
  
  ts_exp_do_not_seek_care <- matrix(rbinom(N * horizon, as.vector(rabies_results$ts_exposures), 1 - pSeek_exposure), N, horizon)
  # sutract?
  
  ts_healthy_seek_care        <- ts_recorded_bites - ts_exp_seek_care
  # we do not have healthy do not seek care (hard to truly know)
  
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
  ts_healthy_start       <- matrix(rbinom(N * horizon, as.vector(ts_recorded_bites),   pStart_healthy),    N, horizon)
  ts_healthy_second_dose <- matrix(rbinom(N * horizon, as.vector(ts_healthy_start),       pCompliance_healthy),N, horizon)
  ts_healthy_complete    <- matrix(rbinom(N * horizon, as.vector(ts_healthy_second_dose), pCompliance_healthy),N, horizon)
  ts_healthy_incomplete  <- ts_healthy_start - ts_healthy_complete
  
  ## RIG
  tmp    <- if (ibcm == "no") ts_exp_start + ts_healthy_start else ts_exp_start
  ts_RIG <- matrix(rbinom(N * horizon, as.vector(tmp), RIG_cov), N, horizon)
  
  # ---------------------------------------------------------------------------#
  # 4: IBCM  ######
  # ---------------------------------------------------------------------------#
  bites_by_each_rabid_biting_dog <- rabies_results$bites_by_dog
  
  #: biters_sought_care — avoid rep() inside loop by pre-expanding once
  #          per cell; still O(N*horizon) iterations but the body is leaner.
  n_cells <- N * horizon
  biters_sought_care <- vector("list", n_cells)
  exp_seek_vec       <- as.integer(ts_exp_seek_care)
  
  for (x in seq_len(n_cells)) {
    b <- bites_by_each_rabid_biting_dog[[x]]
    if (length(b) == 0L) {
      biters_sought_care[[x]] <- integer(0L)
    } else {
      # rep(dog_id, n_bites) then sample — keep original semantics
      biters_sought_care[[x]] <- sample(
        rep.int(seq_along(b), b),
        exp_seek_vec[x]
      )
    }
  }
  
  ts_rabid_bites_investigated <- matrix(
    rbinom(N * horizon, as.vector(ts_exp_seek_care), pInvestigate), N, horizon)
  
  #: investigated biters — same loop structure, slightly tightened
  biters_sought_care_investigated <- vector("list", n_cells)
  n_investigate_vec <- as.integer(ts_rabid_bites_investigated)
  for (x in seq_len(n_cells)) {
    b <- biters_sought_care[[x]]
    ni <- n_investigate_vec[x]
    if (length(b) == 0L || is.na(ni) || ni <= 0L) {
      biters_sought_care_investigated[[x]] <- integer(0L)
    } else {
      biters_sought_care_investigated[[x]] <- b[sample.int(length(b), min(ni, length(b)))]
    }
  }
  
  #dogs investigated via ibcm
  ts_rabid_biting_investigated <- matrix(
    vapply(biters_sought_care_investigated, function(x) length(unique(x)), 1L), nrow = N)
  
  ts_rabid_biting_found    <- matrix(rbinom(N * horizon, as.vector(ts_rabid_biting_investigated), pFound),    N, horizon)
  ts_rabid_biting_testable <- matrix(rbinom(N * horizon, as.vector(ts_rabid_biting_found),        pTestable), N, horizon)
  
  ts_healthy_biting_investigated <- matrix(
    rbinom(N * horizon, as.vector(ts_healthy_seek_care), pFS), N, horizon)
  
  # ---------------------------------------------------------------------------#
  # 7: Outcomes 
  ## 7A: deaths ########
  # ---------------------------------------------------------------------------#

  ts_deaths_no_PEP <- matrix(
    rbinom(N * horizon, as.vector(ts_exp_nostartPEP), pDeath), N, horizon)
  
  deaths_incomplete_PEP <- matrix(
    rbinom(N * horizon, as.vector(ts_exp_incomplete), (1 - pPrevent_incomplete)), N, horizon)
  deaths_complete_PEP   <- matrix(
    rbinom(N * horizon, as.vector(ts_exp_complete),   (1 - pPrevent_complete)),   N, horizon)
  
  ts_deaths <- ts_deaths_no_PEP + deaths_incomplete_PEP + deaths_complete_PEP
  
  ## 7B: Deaths averted  ########
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
  #  8: Economics  ########
  # ---------------------------------------------------------------------------#
  ts_complete_PEP   <- ts_exp_complete  + ts_healthy_complete
  ts_incomplete_PEP <- ts_exp_incomplete + ts_healthy_incomplete
  
  ts_PEP_vials    <- (ts_exp_start + ts_healthy_start) * PEP_vials_per_pt

  # discount costs
  disc <- (1 + discount)^(-(0:(horizon - 1)))
  ts_RIG_cost_per_year <- sweep(ts_RIG        * RIG_cost,                    2, disc, `*`)
  ts_cost_PEP_per_year <- sweep(ts_PEP_vials  * human_vaccine_cost_per_vial, 2, disc, `*`)
  ts_MDV_campaign_cost <- sweep(MDV_campaign_cost,                           2, disc, `*`)
  ts_cost_per_year     <- ts_MDV_campaign_cost + ts_cost_PEP_per_year + ts_RIG_cost_per_year
  
  # ---------------------------------------------------------------------------#
  # 9: Collate & return  #######
  # ---------------------------------------------------------------------------#
  
  # All `ts_` objects from the local environment
  my_list      <- ls(pattern = "^ts_")
  out_matrices <- mget(my_list)
  
  # ts_ elements from rabies_results (already stripped)
  rabies_ts <- rabies_results[grep("^ts_", names(rabies_results))]
  dogs_ts   <- vax_results[grep("^ts_", names(vax_results))]
  
  # CHANGE: burn-in already stripped, so NO second stripping here
  out_matrices <- c(out_matrices, rabies_ts, dogs_ts)
  
  return(out_matrices)
}


# ── Single-session example ───────────────────────────────────────────────────

load_rabies_models()   # <-- call once; cached for the whole session

tmp <- decision_tree(
  N = 5, pop=35000000, HDR=c(17,18), unowned_prop=0.58, horizon=5, discount=0.03, mu = 0.38, k = 0.72, 
  #pBite_healthy = 0.2, pSeek_healthy = 0.2, 
  pStart_healthy = 0.9, pCompliance_healthy = 0.5, bpi =15.3,
  pSeek_exposure = 0.95, pStart_exposure = 0.9, pCompliance_exp = 0.96, pDeath = 0.16, 
  pPrevent_complete = 0.99, pPrevent_incomplete = 0.98, rabies_inc= c(0.0075,0.0125),
  mdv_unowned_budget = NULL,  mdv_owned_budget = NULL, vaccinate_owned_dog_cost = c(2,4), 
  vaccinate_unowned_dog_cost = c(2,4), base_vax_cov_owned = 0.05, target_vax_cov_owned = 0.4, 
  base_vax_cov_unowned = 0.05, target_vax_cov_unowned = 0.4, years_to_target =3,
  pInvestigate = 0.5, pFound = 0.4, pTestable = 0.2, pFS=0.05, RIG_cov =0.4, PEP_vials_per_pt = 0.6,
  human_vaccine_cost_per_vial=5, RIG_cost=70, 
  seed = 123, ibcm = "no", dog_burnin = 3
)

tmp$ts_exposures
tmp$ts_rabid_dogs
