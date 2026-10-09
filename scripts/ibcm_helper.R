

# ---------------------------------------------------------------------------#
# 0: IBCM switch ON/OFF   #######
# ---------------------------------------------------------------------------#

apply_ibcm_effects <- function(
    ibcm, pCompliance_exp, pInvestigate, pFound, pTestable, 
    max_pCompliance_exp = 0.99 #, min_pCompliance_healthy = 0.05
) {
  
  if (!ibcm %in% c("yes", "no")) {
    stop("ibcm must be either 'yes' or 'no'")
  }
  
  
  if (ibcm == "no") {
    return(list(
      pInvestigate = 0, pFound = 0, pTestable = 0, ibcm_efficiency = 0, #reactive_vax_cov = 0, pSeek_exposure = pSeek_exposure, 
      pCompliance_exp = pCompliance_exp#, pCompliance_healthy = pCompliance_healthy, dog_vax_cov = dog_vax_cov
    ))
  }
  
  ibcm_efficiency <- pInvestigate * pFound * pTestable
  
  # A. IBCM increases care-seeking among true rabies exposures -- NOW handle reactively IN simulate_ibcm_dynamics below
  
  # Note: Extra care seeking is only by bite victims of dogs that already "sought care"
  # (ie where biting dog had >1 victim (eg 1.3 victims), 1 victim sought care, triggered IBCM then IBCM sought out the other 0.3) 
  # Bite clusters where no one seeks care are not reached by this approach (this is under contact tracing which differs from IBCM )
  
  # missed_care <- 1 - pSeek_exposure
  # # only bites by same dog
  # pSeek_exposure_new <- pmin((pSeek_exposure + (pInvestigate * missed_care)), max_pSeek_exposure)
  
  # B. IBCM improves completion among true exposures
  # Total completion = pCompliance_exp^2 because the model has two compliance steps (3 doses total).
  exp_incomplete <- 1 - pCompliance_exp^2
  
  target_exp_completion <- pmin(
    pCompliance_exp^2 + ibcm_efficiency * exp_incomplete,
    max_pCompliance_exp^2
  )
  
  pCompliance_exp_new <- sqrt(target_exp_completion)
  
  # C. IBCM reduces unnecessary completion among healthy bite patients
  # healthy_completion <- pCompliance_healthy^2
  # 
  # target_healthy_completion <- pmax(
  #   healthy_completion * (1 - ibcm_efficiency),
  #   min_pCompliance_healthy^2
  # )
  # 
  # pCompliance_healthy_new <- sqrt(target_healthy_completion)
  # 
  # D. IBCM-triggered reactive vaccination
  # This now also happens INSIDE simulate_ibcm_dynamics below -- to be truly reactive & stochastic
  # Below was a previous semi-deterministic approach, now redundant  (2 approaches not too different -- see ./scripts/case_simulation_for_Martha.R)
  
  # Approximation: expected test-triggered responses increase effective dog vaccination coverage before rabies prediction.
  ## Simplified steps:
  ## expected_tests <- dog_pop * rabies_inc * mu * pSeek_exposure * ibcm_efficiency
  ## reactive_dogs_vaccinated <- expected_tests * dogs_per_reactive_vax
  ## Set annual upper limit?
  ## reactive_vax_cov <- reactive_dogs_vaccinated / dog_pop
  
  # rabies_inc_mean <- mean(rabies_inc)
  # reactive_vax_cov <- rabies_inc_mean * mu * pSeek_exposure_new * ibcm_efficiency * dogs_per_reactive_vax
  # # rabid biting dogs 'seeking care'  (rabies_inc * mu * pSeek_exposure_new)
  
  #dog_vax_cov_new <- pmin(dog_vax_cov + reactive_vax_cov, max_dog_vax_cov)
  
  list(
    # pInvestigate = pInvestigate,
    # pFound = pFound,
    # pTestable = pTestable,
    ibcm_efficiency = ibcm_efficiency, # ranges 0-1 but a 3-D space (pInvestigate * pFound * pTestable)
    # reactive_vax_cov = reactive_vax_cov,
    # pSeek_exposure = pSeek_exposure_new,
    # dog_vax_cov = dog_vax_cov_new,
    pCompliance_exp = pCompliance_exp_new#,
    #pCompliance_healthy = pCompliance_healthy_new
    
  )
}




# Dynamic dog-rabies and IBCM simulation -------------------------------------
# Requires HelperFun.R to have been sourced and load_rabies_models() called.

simulate_ibcm_dynamics <- function(
    N, total_horizon, baseline_vax_cov, dog_pop, rabies_inc, mu, k,
    pSeek_exposure, max_pSeek_exposure, pInvestigate, pFound, pTestable,
    dogs_per_reactive_vax, max_reactive_vaccinations, pFS,
    max_dog_vax_cov, seed = NULL, pop_serengeti = 1e5
) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # "Semi Contact tracing" acts only on people who did not initially seek care.
  # Convert the population-level upper bound into the maximum allowable
  # conditional probability of reaching such a non-seeker:
  #
  # pSeek + (1 - pSeek) * pReach <= max_pSeek.

  # Convert coverage to an N x total_horizon matrix. calc_vax_coverage()
  # commonly returns a vector, whereas budget-constrained coverage may be a
  # matrix.
  as_N_by_T <- function(x) {
    if (is.matrix(x)) {
      if (!all(dim(x) == c(N, total_horizon))) {
        stop("A coverage matrix must have dimensions N x total_horizon.")
      }
      return(x)
    }
    if (length(x) == 1L) x <- rep(x, total_horizon)
    if (length(x) != total_horizon) {
      stop("Coverage must be scalar, length total_horizon, or N x total_horizon.")
    }
    matrix(rep(x, each = N), nrow = N, ncol = total_horizon)
  }
  
  baseline_vax_cov <- as_N_by_T(baseline_vax_cov)
  effective_vax_cov <- baseline_vax_cov
  
  # Preserve the population-splitting logic used by predict_dograbies_split().
  ref_dogs <- mean(dog_pop, na.rm = TRUE)
  n_splits <- min(100L, ceiling(ref_dogs / pop_serengeti))
  split_pop <- vapply(
    split_dog_population(dog_pop, n_splits),
    as.numeric,
    numeric(N * total_horizon)
  )
  dim(split_pop) <- c(N, total_horizon, n_splits)
  
  vax_samples <- .rabies_model_cache$vax_samples
  case_samples <- .rabies_model_cache$vax_case_samples
  incidence_adjust <- 0.0001394842
  
  # Draw each subpopulation's model parameters once per iteration, matching
  # predict_cases(): one year-1 draw and one shared draw for subsequent years.
  n_chains <- N * n_splits
  vax_pars <- as.matrix(vax_samples[
    sample.int(nrow(vax_samples), n_chains, replace = TRUE), , drop = FALSE
  ])
  case_pars <- as.matrix(case_samples[
    sample.int(nrow(case_samples), n_chains, replace = TRUE), , drop = FALSE
  ])
  
  previous_cases <- matrix(NA_real_, N, n_splits)
  
  new_mat <- function(mode = "double") {
    matrix(if (mode == "integer") 0L else 0, N, total_horizon)
  }
  ts_rabid_dogs <- new_mat()
  ts_exposures <- new_mat("integer")
  ts_rabid_biting_dogs <- new_mat("integer")
  ts_exp_seek_care_initial <- new_mat("integer")
  ts_rabid_bites_investigated <- new_mat("integer")
  ts_rabid_biting_investigated <- new_mat("integer")
  ts_bites_by_biters_investigated_noSeek <- new_mat("integer")
  ts_bites_reached_by_ibcm <- new_mat("integer")
  ts_rabid_biting_found <- new_mat("integer")
  ts_rabid_biting_tested <- new_mat("integer")
  ts_positive_tests <- new_mat("integer")
  ts_reactive_vax_events <- new_mat("integer")
  ts_dogs_reactive_vaccinated <- new_mat("integer")
  bites_by_dog <- vector("list", N * total_horizon)
  
  # Column-major index keeps list output aligned with as.vector(matrix).
  cell_index <- function(rep, year) rep + (year - 1L) * N
  
  for (year in seq_len(total_horizon)) {
    pop_y <- matrix(split_pop[, year, ], nrow = N, ncol = n_splits)
    cov_y <- effective_vax_cov[, year]
    
    if (year == 1L) {
      eta <- vax_pars[, 1L] + vax_pars[, 2L] * rep(cov_y, n_splits) +
        log(as.vector(pop_y))
      cases_y <- matrix(
        pmin(
          rnbinom(n_chains, mu = exp(eta), size = vax_pars[, 3L]),
          rabies_inc[2L] * as.vector(pop_y)
        ),
        N, n_splits
      )
    } else {
      eta <- case_pars[, 1L] + case_pars[, 2L] * rep(cov_y, n_splits) +
        case_pars[, 3L] * log(previous_cases / pop_y + incidence_adjust) +
        log(pop_y)
      cases_y <- matrix(
        pmin(
          rnbinom(n_chains, mu = exp(as.vector(eta)), size = case_pars[, 4L]),
          rabies_inc[2L] * as.vector(pop_y)
        ),
        N, n_splits
      )
    }
    previous_cases <- cases_y
    ts_rabid_dogs[, year] <- rowSums(cases_y)
    
    # Dog-level bite attribution is required for co-victim contact tracing.
    for (rep in seq_len(N)) {
      b <- nBitesBiters(
        as.integer(ts_rabid_dogs[rep, year]), pBite = mu, pBiteK = k
      )$bites_by_dog
      idx <- cell_index(rep, year)
      bites_by_dog[[idx]] <- b
      
      n_exp <- sum(b)
      ts_exposures[rep, year] <- n_exp
      ts_rabid_biting_dogs[rep, year] <- length(b)
      if (n_exp == 0) next
      
      n_seek <- rbinom(1, n_exp, pSeek_exposure)
      ts_exp_seek_care_initial[rep, year] <- n_seek
      if (n_seek == 0) next
      
      # Sample victims without replacement, retaining their biting-dog IDs.
      seeker_biters <- sample(rep.int(seq_along(b), b), n_seek)
      n_inv_bites <- rbinom(1, n_seek, pInvestigate)
      ts_rabid_bites_investigated[rep, year] <- n_inv_bites
      if (n_inv_bites == 0) next
      
      investigated_biters <- seeker_biters[
        sample.int(n_seek, n_inv_bites, replace = FALSE)
      ]
      investigated_dogs <- unique(investigated_biters)
      n_inv_dogs <- length(investigated_dogs)
      ts_rabid_biting_investigated[rep, year] <- n_inv_dogs
      
      # All bites from investigated dogs minus their victims already in care.
      missed <- sum(b[investigated_dogs]) -
        sum(seeker_biters %in% investigated_dogs)
      missed <- max(0, as.integer(missed))
      ts_bites_by_biters_investigated_noSeek[rep, year] <- missed
      
      
      # pInvestigate has already been applied when selecting investigated bites.
      # Reach the remaining victims of investigated dogs, subject to the maximum
      # allowable exposure care-seeking level.
      max_seekers <- floor(max_pSeek_exposure * n_exp)
      
      remaining_capacity <- max(
        0L,
        max_seekers - n_seek
      )
      
      ts_bites_reached_by_ibcm[rep, year] <- min(missed,remaining_capacity)
      
      
      n_found <- rbinom(1, n_inv_dogs, pFound)
      n_tested <- rbinom(1, n_found, pTestable)
      
      # All dogs in this pathway are truly rabid; assume a perfect diagnostic test
      n_positive <- n_tested
      
      ts_rabid_biting_found[rep, year] <- n_found
      ts_rabid_biting_tested[rep, year] <- n_tested
      ts_positive_tests[rep, year] <- n_positive
      
      events <- min(n_positive, max_reactive_vaccinations)
      ts_reactive_vax_events[rep, year] <- events
      
      
      # Count only dogs that can add effective coverage in the year that
      # benefits. This implements the no-revaccination assumption and avoids
      # charging for nominal vaccinations above the coverage ceiling.
      benefit_year <- min(year + 1, total_horizon)
      reactive_dogs <- min(
        events * dogs_per_reactive_vax,
        floor(dog_pop[rep, benefit_year] *
                (max_dog_vax_cov - baseline_vax_cov[rep, benefit_year]))
      )
      ts_dogs_reactive_vaccinated[rep, year] <- max(0L, reactive_dogs)
    }
    
    # A response triggered in year t affects year t+1 only. Coverage is added
    # to the next year's underlying MDV effort, never to the current year.
    if (year < total_horizon) {
      effective_vax_cov[, year + 1L] <- pmin(
        max_dog_vax_cov,
        baseline_vax_cov[, year + 1L] +
          ts_dogs_reactive_vaccinated[, year] / dog_pop[, year + 1L]
      )
    }
  }
  
  list(
    ts_rabid_dogs = ts_rabid_dogs,
    ts_exposures = ts_exposures,
    ts_rabid_biting_dogs = ts_rabid_biting_dogs,
    bites_by_dog = bites_by_dog,
    ts_exp_seek_care_initial = ts_exp_seek_care_initial,
    ts_rabid_bites_investigated = ts_rabid_bites_investigated,       # people
    ts_rabid_biting_investigated = ts_rabid_biting_investigated,     # dogs
    #ts_dog_investigations = ts_dog_investigations,                   # all (healthy + rabid)
    ts_bites_by_biters_investigated_noSeek = ts_bites_by_biters_investigated_noSeek,             # missed
    ts_bites_reached_by_ibcm = ts_bites_reached_by_ibcm, # min(missed,remaining_capacity)
    ts_rabid_biting_found = ts_rabid_biting_found,
    ts_rabid_biting_tested = ts_rabid_biting_tested,
    ts_positive_tests = ts_positive_tests,
    ts_reactive_vax_events = ts_reactive_vax_events,
    ts_dogs_reactive_vaccinated = ts_dogs_reactive_vaccinated,
    ts_dog_vax_cov_baseline = baseline_vax_cov,
    ts_dog_vax_cov_effective = effective_vax_cov
  )
}
