# Wrapper for the IBCM and no-IBCM decision-tree models -----------------------
#
# Source order:
source("./scripts/decision_tree_no_ibcm.R")
source("./scripts/decision_tree_ibcm.R")
# source("./scripts/decision_tree_wrapper.R")
# load_rabies_models()  # once, after all model scripts have been sourced


decision_tree_wrapper <- function(ibcm = "no", baseline_surveillance = 0.01, ..., .standardise_outputs = TRUE, .quiet = FALSE) {
  
  # Accept TRUE/FALSE as well as "yes"/"no"
  if (is.logical(ibcm) && length(ibcm) == 1L && !is.na(ibcm)) ibcm <- if (ibcm) "yes" else "no"
  if (!is.character(ibcm) || length(ibcm) != 1L || is.na(ibcm)) stop("`ibcm` must be one of \"yes\", \"no\", TRUE, or FALSE.", call. = FALSE)
  ibcm <- match.arg(tolower(ibcm), c("yes", "no"))
  
  model_fun <- if (ibcm == "yes") decision_tree_ibcm else decision_tree_no_ibcm
  
  args <- list(...)
  if (length(args) && (is.null(names(args)) || any(names(args) == ""))) stop("All model arguments supplied through `...` must be named.", call. = FALSE)
  
  valid_args <- names(formals(model_fun))
  unknown_args <- setdiff(names(args), valid_args)
  
  ibcm_only_args <- c(
    "pInvestigate", "pFound", "pTestable", "pFS", "dogs_per_reactive_vax",
    "max_pSeek_exposure", "max_pCompliance_exp", #"min_pCompliance_healthy",
    "max_dog_vax_cov", "max_reactive_vaccinations",
    "reactive_vax_cost_per_dog", "investigation_cost_per_dog",
    "dog_test_cost_per_dog", "ibcm_increase_exposure_care_seeking",
    "ibcm_increase_exposure_compliance", #"ibcm_decrease_healthy_compliance",
    "ibcm_reactive_vaccination"
  )
  
  invalid_args <- if (ibcm == "no") setdiff(unknown_args, ibcm_only_args) else unknown_args
  if (length(invalid_args)) stop("Unknown model argument(s): ", paste(invalid_args, collapse = ", "), call. = FALSE)
  
  ignored_args <- if (ibcm == "no") intersect(names(args), ibcm_only_args) else character(0)
  if (length(ignored_args) && !.quiet) warning("Ignoring IBCM-only argument(s) because `ibcm = \"no\"`: ", paste(ignored_args, collapse = ", "), call. = FALSE)
  args[ignored_args] <- NULL
  
  out <- do.call(model_fun, args)
  
  if (!is.list(out)) stop("The selected model must return a named list.", call. = FALSE)
  if (is.null(out$ts_exposures) || !is.matrix(out$ts_exposures)) stop("The selected model did not return a matrix named `ts_exposures`.", call. = FALSE)
  
  if (.standardise_outputs) {
    
    zero <- matrix(0, nrow = nrow(out$ts_exposures), ncol = ncol(out$ts_exposures))
    
    if (ibcm == "no") {
      
      if (is.null(out$ts_exp_seek_care) || !is.matrix(out$ts_exp_seek_care)) stop("The no-IBCM model did not return `ts_exp_seek_care` as a matrix.", call. = FALSE)
      if (is.null(out$ts_rabid_dogs) || !is.matrix(out$ts_rabid_dogs) || !identical(dim(out$ts_rabid_dogs), dim(out$ts_exposures))) {
        stop("The no-IBCM model must return `ts_rabid_dogs` as an N-by-horizon matrix.", call. = FALSE)
      }
      
      out$ts_exp_seek_care_initial <- out$ts_exp_seek_care
      
      # Baseline dog surveillance in settings without IBCM:
      # each rabid dog has probability `baseline_surveillance` of being tested.
      out$ts_dogs_tested <- matrix(
        rbinom(length(out$ts_rabid_dogs), size = as.vector(round(out$ts_rabid_dogs)), prob = baseline_surveillance),
        nrow = nrow(out$ts_rabid_dogs),
        ncol = ncol(out$ts_rabid_dogs)
      )
      
      out$ts_positive_tests <- out$ts_dogs_tested
      
      # IBCM-specific processes remain zero when IBCM is absent
      zero_names <- c(
        "ts_rabid_bites_investigated",
        "ts_rabid_biting_investigated",
        "ts_healthy_biting_investigated",
        "ts_rabid_biting_found",
        "ts_rabid_biting_tested",
        "ts_healthy_biting_found",
        "ts_healthy_biting_tested",
        "ts_dog_investigations",
        "ts_bites_by_biters_investigated_noSeek",
        "ts_bites_reached_by_ibcm",
        "ts_dogs_found",
        "ts_reactive_vax_events",
        "ts_dogs_reactive_vaccinated",
        "ts_reactive_vax_cost",
        "ts_investigation_cost",
        "ts_dog_testing_cost",
        "ts_ibcm_costs"
      )
      
      for (nm in zero_names) out[[nm]] <- zero
      
    } else {
      
      
      required_ibcm_outputs <- c(
        "ts_exp_seek_care_initial",
        "ts_bites_reached_by_ibcm",
        "ts_dog_investigations",
        "ts_rabid_biting_found",
        "ts_rabid_biting_tested",
        "ts_healthy_biting_found",
        "ts_healthy_biting_tested",
        "ts_dogs_found",
        "ts_dogs_tested",
        "ts_positive_tests",
        "ts_dogs_reactive_vaccinated",
        "ts_reactive_vax_cost",
        "ts_investigation_cost",
        "ts_dog_testing_cost",
        "ts_ibcm_costs"
      )
      
      missing_outputs <- required_ibcm_outputs[
        !vapply(required_ibcm_outputs, function(nm) is.matrix(out[[nm]]) && identical(dim(out[[nm]]), dim(out$ts_exposures)), logical(1))
      ]
      
      if (length(missing_outputs)) {
        stop(paste0("The IBCM model is missing required N-by-horizon output(s): ", paste(missing_outputs, collapse = ", "), ". Check that the revised decision_tree_ibcm.R was sourced."), call. = FALSE)
      }
    }
  }
  
  attr(out, "ibcm") <- ibcm
  attr(out, "baseline_surveillance") <- baseline_surveillance
  
  out
}


