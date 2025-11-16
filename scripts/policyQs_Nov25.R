


# Quick analysis WHO 14 th Nov 25


source("./scripts/stochastic_decision_tree.R")


params <- read.csv("./data/policy_params.csv")


# Run scenarios per country
run_country <- function(params_df, country_name) {
  
  params_country <- params_df %>%
    dplyr::filter(
      Country==country_name
    )
  
  results <- list()
  
  message("\n========== Running scenarios for ", country_name, " ==========\n")
  
  for (i in seq_len(nrow(params_country))) {
    
    row <- params_country[i, ]
    scenario_name <- row$scenarios
    
    message("→ Scenario: ", scenario_name)
    
    # ---- RUN DECISION TREE USING YOUR RULES ----
    out <- decision_tree(
      N = 1000,
      pop = row$Population,
      HDR = c(row$HDR, row$HDR),
      horizon = row$horizon,
      
      # human exposure parameters
      pBite_healthy      = row$pBite_healthy,
      pSeek_healthy      = row$pSeek_healthy,
      pStart_healthy     = row$pStart_healthy,
      pComplete_healthy  = row$pComplete_healthy,
      
      pSeek_exposure     = row$pSeek_exposure,
      pStart_exposure    = row$pStart_exposure,
      pComplete_exposure = row$pComplete_exposure,
      
      # disease outcome & PEP parameters
      pDeath       = row$pDeath,
      pPrevent     = row$pPrevent,
      full_cost    = row$full_cost,
      partial_cost = row$partial_cost,
      
      # MDV campaign
      mdv_campaign_budget = NULL,
      base_vax_cov        = row$base_vax_cov,
      vaccinate_dog_cost  = row$vaccinate_dog_cost,
      target_vax_cov      = row$target_vax_cov,
      
      # surveillance parameters
      pInvestigate = row$pInvestigate,
      pFound       = row$pFound,
      pTestable    = row$pTestable,
      pFP          = row$pFP
    )
    
    # store output
    results[[scenario_name]] <- out
  }
  
  return(results)
}

# summarise a given matrix across scenarios

# summarise function
summarise_across_scenarios <- function(country_results, matrix_names) {
  
  output_list <- list()
  
  for (mat_name in matrix_names) {
    
    scen_summaries <- lapply(names(country_results), function(scenario) {
      
      mat <- country_results[[scenario]][[mat_name]]
      
      df <- summarise_stochasticity(mat, scenario = scenario)
      df$scenario <- scenario
      df$matrix   <- mat_name    # optional label
      
      df
    })
    
    # bind scenarios for this matrix
    combined <- dplyr::bind_rows(scen_summaries)
    
    # store under matrix name
    output_list[[mat_name]] <- combined
  }
  
  return(output_list)
}


# run per country
tz_results  <- run_country(params,  "Tanzania")
mlw_results <- run_country(params, "Malawi")
ug_results  <- run_country(params,  "Uganda")
ke_results  <- run_country(params,  "Kenya")

# summarise across scnerios
tz_summary <- summarise_across_scenarios(
  country_results = tz_results,
  matrix_names = c("ts_deaths", "ts_deaths_averted", "ts_start_PEP")
)

mlw_summary <- summarise_across_scenarios(
  country_results = mlw_results,
  matrix_names = c("ts_deaths", "ts_deaths_averted", "ts_start_PEP")
)

ug_summary <- summarise_across_scenarios(
  country_results = ug_results,
  matrix_names = c("ts_deaths", "ts_deaths_averted", "ts_start_PEP")
)

ke_summary <- summarise_across_scenarios(
  country_results = ke_results,
  matrix_names = c("ts_deaths", "ts_deaths_averted", "ts_start_PEP")
)

# Calculate vials used
## Using Luka et al, 70% are low throughput ( vials per patient), 20% medium and 10% high
  ## Assuming no wastage; - 2.5 vials per patient in low throughput, 
                        # - 1.8 vials per patient in medium throughput
                        # - 1.2 vials per patient in high throughput

# For simplicity, average vials per patient is 2.23 (assuming no wastage)
# We also assume everyone that starts, completes PEP
(0.7 * 2.5) + (0.2 * 1.8) + (0.1 * 1.2)

# Function to add vials
add_vials_used <- function(summary_list, multiplier = 2.23) {
  
  if (!"ts_start_PEP" %in% names(summary_list)) {
    stop("ts_start_PEP not found in summary_list")
  }
  
  df <- summary_list$ts_start_PEP
  
  vials_df <- df %>%
    dplyr::mutate(
      LL     = LL     * multiplier,
      Median = Median * multiplier,
      UL     = UL     * multiplier,
      matrix = "ts_vials_used"
    )
  
  summary_list$ts_vials_used <- vials_df
  
  return(summary_list)
}

# Add the vials df
tz_summary <- add_vials_used(tz_summary, 2.23)
mlw_summary <- add_vials_used(mlw_summary, 2.23)
ug_summary <- add_vials_used(ug_summary, 2.23)
ke_summary <- add_vials_used(ke_summary, 2.23)


# Save to share
save(
  tz_summary,
  mlw_summary,
  ug_summary,
  ke_summary,
  file = "./output/output_summaries_Nov2025.rda"
)

# To load
load("./output/output_summaries_Nov2025.rda")




