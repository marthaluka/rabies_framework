
# One-way sensitivity analysis

# source model
source("./scripts/decision_tree_wrapper.R")

# Parallel-processing configuration
future::plan(future::multisession, workers = 8)


# 1. Fixed model arguments ########
# -----------------------------------------------------------------------------

fixed_args <- list(
  N = 100,
  horizon = 10,
  discount = 0,
  
  mu = 0.38,
  k = 0.72,
  rabies_inc = c(0.0075, 0.0125),
  
  pDeath = 0.17,
  pPrevent_complete = 0.999,
  pPrevent_incomplete = 0.986,
  
  mdv_unowned_budget = NULL,
  mdv_owned_budget = NULL,
  years_to_target = 3,
  
  seed = 123,
  dog_burnin = 1,
  
  ibcm_increase_exposure_care_seeking = TRUE,
  ibcm_increase_exposure_compliance = TRUE,
  ibcm_decrease_healthy_compliance = TRUE,
  ibcm_reactive_vaccination = TRUE,
  
  .standardise_outputs = TRUE,
  .quiet = TRUE
)

# 
# default_selected_vars <- c(
#   "ts_deaths",
#   "ts_deaths_averted",
#   "ts_rabid_dogs",
#   "ts_exposures",
#   "ts_cost_per_year"
# )


# 2. Reusable functions ##########
# -----------------------------------------------------------------------------


# Convert one CSV row into model arguments
row_to_model_args <- function(
    row,
    fixed_args,
    vector_columns = list()
) {
  row <- as.list(row)
  
  # Reconstruct vector-valued model arguments
  for (arg in names(vector_columns)) {
    cols <- vector_columns[[arg]]
    
    row[[arg]] <- as.numeric(
      unlist(row[cols], use.names = FALSE)
    )
    
    row[cols] <- NULL
  }
  
  # Remove scenario metadata
  row$scenario <- NULL
  
  # Standardise wrapper input
  row$ibcm <- tolower(as.character(row$ibcm))
  
  # Convert IBCM switches to logical values
  logical_args <- intersect(
    c(
      "ibcm_increase_exposure_care_seeking",
      "ibcm_increase_exposure_compliance",
      "ibcm_decrease_healthy_compliance",
      "ibcm_reactive_vaccination"
    ),
    names(row)
  )
  
  row[logical_args] <- lapply(
    row[logical_args],
    as.logical
  )
  
  # CSV values override fixed values
  utils::modifyList(fixed_args, row)
}


# Run one scenario and summarise only selected outputs
run_one_scenario <- function(
    row,
    fixed_args,
    selected_vars,
    vector_columns = list()
) {
  
  scenario <- as.character(row$scenario)
  
  model_args <- row_to_model_args(
    row = row,
    fixed_args = fixed_args,
    vector_columns = vector_columns
  )
  
  output <- do.call(
    decision_tree_wrapper,
    model_args
  )
  
  cumulative <- purrr::map_dfr(
    selected_vars,
    \(var) {
      summarise_across_horizon(output[[var]]) |>
        dplyr::mutate(
          scenario = scenario,
          outcome = var,
          .before = 1
        )
    }
  )
  
  annual <- purrr::map_dfr(
    selected_vars,
    \(var) {
      summarise_stochasticity(
        mat = output[[var]],
        scenario = scenario
      ) |>
        dplyr::mutate(
          outcome = var,
          .before = 1
        )
    }
  )
  
  list(
    cumulative = cumulative,
    annual = annual
  )
}

# Run every row of a sensitivity parameter table in parallel
run_sensitivity_parallel <- function(
    parameters,
    fixed_args,
    selected_vars = default_selected_vars,
    vector_columns = list()
) {
  
  parameter_rows <- split(
    parameters,
    seq_len(nrow(parameters))
  )
  
  results <- furrr::future_map(
    parameter_rows,
    \(row) {
      
      load_rabies_models()
      
      run_one_scenario(
        row = row,
        fixed_args = fixed_args,
        selected_vars = selected_vars,
        vector_columns = vector_columns
      )
    },
    .options = furrr::furrr_options(
      seed = TRUE,
      scheduling = Inf
    ),
    .progress = TRUE
  )
  
  list(
    cumulative = purrr::map_dfr(results, "cumulative"),
    annual = purrr::map_dfr(results, "annual")
  )
}


vector_columns <- list(
  HDR = c(
    "HDR1",
    "HDR2"
  ),
  
  vaccinate_owned_dog_cost = c(
    "vaccinate_owned_dog_cost1",
    "vaccinate_owned_dog_cost2"
  ),
  
  vaccinate_unowned_dog_cost = c(
    "vaccinate_unowned_dog_cost1",
    "vaccinate_unowned_dog_cost2"
  )
)

# 3. HDR sensitivity #######
# -----------------------------------------------------------------------------

hdr_parameters <- read.csv(
  "./data/sensitivity_params/01_hdr_sensitivity.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

hdr_selected_vars <- c(
  "ts_deaths",
  "ts_deaths_averted",
  "ts_deaths_averted_MDV",
  "ts_rabid_dogs",
  "ts_exposures",
  "ts_cost_per_year"
)

hdr_results <- run_sensitivity_parallel(
  parameters = hdr_parameters,
  fixed_args = fixed_args,
  selected_vars = hdr_selected_vars,
  vector_columns = vector_columns
)


names(hdr_results)

hdr_results$annual %>%
  dplyr::filter(outcome == "ts_deaths")

hdr_results$cumulative %>%
  dplyr::filter(outcome == "ts_deaths")

saveRDS(hdr_results, file = "./output/sensitivity_analysis/hdr_results.rds")

# 4. MDV sensitivity #######
# -----------------------------------------------------------------------------

mdv_parameters <- read.csv(
  "./data/sensitivity_params/02_mdv_sensitivity.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

mdv_selected_vars <- c(
  "ts_deaths",
  "ts_deaths_averted",
  "ts_deaths_averted_MDV",
  "ts_rabid_dogs",
  "ts_exposures",
  "ts_cost_per_year"
)

mdv_results <- run_sensitivity_parallel(
  parameters = mdv_parameters,
  fixed_args = fixed_args,
  selected_vars = mdv_selected_vars,
  vector_columns = vector_columns
)


names(mdv_results)

mdv_results$annual %>%
  dplyr::filter(outcome == "ts_deaths")

mdv_results$cumulative %>%
  dplyr::filter(outcome == "ts_deaths")

saveRDS(mdv_results, file = "./output/sensitivity_analysis/mdv_results.rds")



# 5. pSeek sensitivity #######
# -----------------------------------------------------------------------------


pSeek_parameters <- read.csv(
  "./data/sensitivity_params/03_pSeek_sensitivity.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Outcomes most relevant to care seeking
pSeek_selected_vars <- c(
  "ts_exp_seek_care",
  "ts_exposures",
  "ts_deaths_averted_PEP",
  "ts_deaths",
  "ts_deaths_averted",
  "ts_cost_PEP_per_year"
)

pSeek_results <- run_sensitivity_parallel(
  parameters = pSeek_parameters,
  fixed_args = fixed_args,
  selected_vars = pSeek_selected_vars,
  vector_columns = vector_columns
)

names(pSeek_results)

pSeek_results$cumulative
pSeek_results$annual

saveRDS(pSeek_results, file = "./output/sensitivity_analysis/pSeek_results.rds")


# 6. pStart sensitivity #######
# -----------------------------------------------------------------------------


pStart_parameters <- read.csv(
  "./data/sensitivity_params/04_pStart_sensitivity.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Outcomes most relevant to care seeking
pStart_selected_vars <- c(
  "ts_exp_start",
  "ts_exposures",
  "ts_deaths_averted_PEP",
  "ts_deaths",
  "ts_deaths_averted",
  "ts_cost_PEP_per_year"
)

pStart_results <- run_sensitivity_parallel(
  parameters = pStart_parameters,
  fixed_args = fixed_args,
  selected_vars = pStart_selected_vars,
  vector_columns = vector_columns
)

names(pStart_results)

pStart_results$cumulative
pStart_results$annual

saveRDS(pStart_results, file = "./output/sensitivity_analysis/pStart_results.rds")


# 7. pCompliance sensitivity #######
# -----------------------------------------------------------------------------


pComplete_parameters <- read.csv(
  "./data/sensitivity_params/05_pCompliance_sensitivity.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Outcomes most relevant to care seeking
pComplete_selected_vars <- c(
  "ts_exp_complete",
  "ts_exposures",
  "ts_deaths_averted_PEP",
  "ts_deaths",
  "ts_deaths_averted",
  "ts_cost_PEP_per_year"
)

pComplete_results <- run_sensitivity_parallel(
  parameters = pComplete_parameters,
  fixed_args = fixed_args,
  selected_vars = pComplete_selected_vars,
  vector_columns = vector_columns
)

names(pComplete_results)

pComplete_results$cumulative
pComplete_results$annual

saveRDS(pComplete_results, file = "./output/sensitivity_analysis/pComplete_results.rds")

# 8. IBCM sensitivity #######

  # pSeek exp
  # pComplete healthy
  # pComplete exposure
  # reactive vaccination
  # dog testing


## 8A. pInvestigate sensitivity  #######
pInvestigate_parameters <- read.csv(
  "./data/sensitivity_params/06_pInvestigate_sensitivity.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Outcomes most relevant to care seeking
pInvestigate_selected_vars <- c(
  "ts_exp_seek_care",
  "ts_exp_complete",
  "ts_healthy_complete",
  "ts_bites_reached_by_ibcm",
  "ts_rabid_biting_found",
  "ts_dogs_reactive_vaccinated",
  "ts_dogs_tested",
  "ts_deaths",
  "ts_deaths_averted",
  "ts_cost_PEP_per_year",
  "ts_ibcm_costs"
)

pInvestigate_results <- run_sensitivity_parallel(
  parameters = pInvestigate_parameters,
  fixed_args = fixed_args,
  selected_vars = pInvestigate_selected_vars,
  vector_columns = vector_columns
)

names(pInvestigate_results)

pInvestigate_results$cumulative
pInvestigate_results$annual

saveRDS(pInvestigate_results, file = "./output/sensitivity_analysis/pInvestigate_results.rds")



## 8B. pFound sensitivity  #######


pFound_parameters <- read.csv(
  "./data/sensitivity_params/07_pFound_sensitivity.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Outcomes most relevant to care seeking
pFound_selected_vars <- c(
  "ts_exp_seek_care",
  "ts_exp_complete",
  "ts_healthy_complete",
  "ts_bites_reached_by_ibcm",
  "ts_rabid_biting_found",
  "ts_rabid_biting_testable",
  "ts_dogs_reactive_vaccinated",
  "ts_dogs_tested",
  "ts_deaths",
  "ts_deaths_averted",
  "ts_cost_PEP_per_year",
  "ts_ibcm_costs"
)

pFound_results <- run_sensitivity_parallel(
  parameters = pFound_parameters,
  fixed_args = fixed_args,
  selected_vars = pFound_selected_vars,
  vector_columns = vector_columns
)

names(pFound_results)

pFound_results$cumulative
pFound_results$annual

saveRDS(pFound_results, file = "./output/sensitivity_analysis/pFound_results.rds")



## 8C. pTestable sensitivity  #######


pTestable_parameters <- read.csv(
  "./data/sensitivity_params/08_pTestable_sensitivity.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Outcomes most relevant to care seeking
pTestable_selected_vars <- c(
  "ts_exp_seek_care",
  "ts_exp_complete",
  "ts_healthy_complete",
  "ts_bites_reached_by_ibcm",
  "ts_rabid_biting_testable",
  "ts_dogs_reactive_vaccinated",
  "ts_dogs_tested",
  "ts_deaths",
  "ts_cost_PEP_per_year",
  "ts_deaths_averted",
  "ts_ibcm_costs"
  )



pTestable_results <- run_sensitivity_parallel(
  parameters = pTestable_parameters,
  fixed_args = fixed_args,
  selected_vars = pTestable_selected_vars,
  vector_columns = vector_columns
)

names(pTestable_results)

pTestable_results$cumulative
pTestable_results$annual

saveRDS(pTestable_results, file = "./output/sensitivity_analysis/pTestable_results.rds")




# To Do 

# Review ibcm model to reduce redundancy

    ## ts_dogs_tested vs ts_rabid_biting_testable
    ## ts_bites_reached_by_ibcm vs ts_bites_by_biters_investigated_noSeek
    ## Avoid the n_missed, n_found etc variables -- adds unnecessary weight to running model


# 9. Visualize ########

pacman::p_load(
  tidyverse, patchwork, cowplot
)

# Read data
hdr_results <- readRDS("./output/sensitivity_analysis/hdr_results.rds")
mdv_results <- readRDS("./output/sensitivity_analysis/mdv_results.rds")
pSeek_results <- readRDS("./output/sensitivity_analysis/pSeek_results.rds")
pStart_results <- readRDS( "./output/sensitivity_analysis/pStart_results.rds")
pComplete_results <- readRDS("./output/sensitivity_analysis/pComplete_results.rds")
pComplete_results <- readRDS("./output/sensitivity_analysis/pComplete_results.rds")
pInvestigate_results <- readRDS("./output/sensitivity_analysis/pInvestigate_results.rds")
pFound_results <- readRDS("./output/sensitivity_analysis/pFound_results.rds")
pTestable_results <- readRDS("./output/sensitivity_analysis/pTestable_results.rds")



names(hdr_results)

hdr_results$annual
hdr_results$cumulative

# plot function: both annual and cumulative
# Plot annual and cumulative sensitivity-analysis results --------------------

plot_sensitivity_output <- function(
    results,
    outcome,
    title,
    scenarios = NULL,
    facet_ncol = 3
) {
  
  annual <- results$annual[
    results$annual$outcome == outcome,
  ]
  
  cumulative <- results$cumulative[
    results$cumulative$outcome == outcome,
  ]
  
  # Optional scenario order; otherwise retain the order in the results.
  if (is.null(scenarios)) {
    scenarios <- unique(annual$scenario)
  }
  
  annual$scenario <- factor(annual$scenario, levels = scenarios)
  cumulative$scenario <- factor(cumulative$scenario, levels = scenarios)
  
  temporal_plot <- ggplot(
    annual,
    aes(
      x = year,
      y = Median,
      group = scenario,
      colour = scenario,
      fill = scenario
    )
  ) +
    geom_ribbon(
      aes(ymin = LL, ymax = UL),
      alpha = 0.25,
      colour = NA
    ) +
    geom_line(linewidth = 0.7) +
    facet_wrap(~scenario, ncol = facet_ncol) +
    labs(
      title = paste(title, "with 95% uncertainty intervals"),
      x = "Year",
      y = title
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme_bw() +
    theme(legend.position = "none")
  
  cumulative_plot <- ggplot(
    cumulative,
    aes(x = scenario, y = Median, colour = scenario)
  ) +
    geom_point(size = 2) +
    geom_errorbar(
      aes(ymin = LL, ymax = UL),
      width = 0.15
    ) +
    labs(
      x = "Scenario",
      y = paste("Cumulative", title)
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  
  temporal_plot + cumulative_plot
}





















## 1. HDR #########
plot_sensitivity_output(
  results = hdr_results,
  outcome = "ts_deaths",
  title = "Deaths",
)

plot_sensitivity_output(
  results = hdr_results,
  outcome = "ts_rabid_dogs",
  title = "Rabid dogs",
)

plot_sensitivity_output(
  results = hdr_results,
  outcome = "ts_deaths_averted_MDV",
  title = "Deaths averted MDV",
)

plot_sensitivity_output(
  results = hdr_results,
  outcome = "ts_cost_per_year",
  title = "Costs",
)

## 2. MDV #########
plot_sensitivity_output(
  results = mdv_results,
  outcome = "ts_deaths",
  title = "Deaths",
)

plot_sensitivity_output(
  results = mdv_results,
  outcome = "ts_rabid_dogs",
  title = "Rabid dogs",
)

plot_sensitivity_output(
  results = mdv_results,
  outcome = "ts_deaths_averted_MDV",
  title = "Deaths averted MDV",
)

plot_sensitivity_output(
  results = mdv_results,
  outcome = "ts_cost_per_year",
  title = "Costs",
)

## 3. pSeek #########
plot_sensitivity_output(
  results = pSeek_results,
  outcome = "ts_exp_seek_care",
  title = "Exp seek care",
)

plot_sensitivity_output(
  results = pSeek_results,
  outcome = "ts_deaths_averted_PEP",
  title = "Deaths averted PEP",
)

plot_sensitivity_output(
  results = pSeek_results,
  outcome = "ts_cost_PEP_per_year",
  title = "PEP costs",
)


plot_sensitivity_output(
  results = pSeek_results,
  outcome = "ts_deaths",
  title = "Deaths",
)



## 4. pStart #########
plot_sensitivity_output(
  results = pStart_results,
  outcome = "ts_exp_start",
  title = "Exp start care",
)

plot_sensitivity_output(
  results = pStart_results,
  outcome = "ts_deaths_averted_PEP",
  title = "Deaths averted PEP",
)

plot_sensitivity_output(
  results = pStart_results,
  outcome = "ts_cost_PEP_per_year",
  title = "PEP costs",
)

plot_sensitivity_output(
  results = pStart_results,
  outcome = "ts_deaths",
  title = "Deaths",
)

## 5. pCompliance #########
plot_sensitivity_output(
  results = pComplete_results,
  outcome = "ts_exp_complete",
  title = "Exp start complete",
)

plot_sensitivity_output(
  results = pComplete_results,
  outcome = "ts_deaths_averted_PEP",
  title = "Deaths averted PEP",
)

plot_sensitivity_output(
  results = pComplete_results,
  outcome = "ts_cost_PEP_per_year",
  title = "PEP costs",
)

plot_sensitivity_output(
  results = pComplete_results,
  outcome = "ts_deaths",
  title = "Deaths",
)


## 6. pInvestigate #########
plot_sensitivity_output(
  results = pInvestigate_results,
  outcome = "ts_healthy_complete",
  title = "Healthy complete PEP",
)

plot_sensitivity_output(
  results = pInvestigate_results,
  outcome = "ts_bites_reached_by_ibcm",
  title = "Additional victims sought out",
)

plot_sensitivity_output(
  results = pInvestigate_results,
  outcome = "ts_ibcm_costs",
  title = "IBCM costs",
)


plot_sensitivity_output(
  results = pInvestigate_results,
  outcome = "ts_dogs_tested",
  title = "Dogs tested",
)

plot_sensitivity_output(
  results = pInvestigate_results,
  outcome = "ts_deaths",
  title = "Deaths",
)

## 7. pFound #########
plot_sensitivity_output(
  results = pFound_results,
  outcome = "ts_healthy_complete",
  title = "Healthy complete PEP",
)

plot_sensitivity_output(
  results = pFound_results,
  outcome = "ts_bites_reached_by_ibcm",
  title = "Additional victims sought out",
)

plot_sensitivity_output(
  results = pFound_results,
  outcome = "ts_ibcm_costs",
  title = "IBCM costs",
)


plot_sensitivity_output(
  results = pFound_results,
  outcome = "ts_dogs_tested",
  title = "Dogs tested",
)


## 8. pTestable #########
plot_sensitivity_output(
  results = pTestable_results,
  outcome = "ts_healthy_complete",
  title = "Healthy complete PEP",
)

plot_sensitivity_output(
  results = pTestable_results,
  outcome = "ts_bites_reached_by_ibcm",
  title = "Additional victims sought out",
)

plot_sensitivity_output(
  results = pTestable_results,
  outcome = "ts_cost_PEP_per_year",
  title = "PEP costs",
)


plot_sensitivity_output(
  results = pTestable_results,
  outcome = "ts_deaths",
  title = "Deaths",
)

plot_sensitivity_output(
  results = pTestable_results,
  outcome = "ts_ibcm_costs",
  title = "IBCM costs",
)

plot_sensitivity_output(
  results = pTestable_results,
  outcome = "ts_dogs_tested",
  title = "Dogs tested",
)



