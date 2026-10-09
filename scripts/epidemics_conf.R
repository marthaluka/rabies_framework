
# Epidemics conference 2025 poster analysis
source("./scripts/stochastic_decision_tree.R")

params <- read.csv("./data/epidemics_params.csv") 


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
      vaccinate_dog_cost  = c(row$vaccinate_dog_cost, row$vaccinate_dog_cost),
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
tz_results  <- run_country(params,  "Tanzania") # Serengeti district (100k dogs)


# summarise across scnerios
tz_summary <- summarise_across_scenarios(
  country_results = tz_results,
  matrix_names = c("ts_deaths", "ts_deaths_averted", "ts_start_PEP")
)

## Deaths #####

df_deaths<- tz_summary$ts_deaths %>%
  dplyr::mutate(scenario = dplyr::recode(
    scenario,
    SQ = "Status Quo",
    imp_access = "Improved PEP access",
    imp_access_modMDV = "Improved PEP + mod MDV",
    imp_access_MDV = "Improved PEP + MDV"
  )) 



scenario_order2 <- c(
  "Status Quo",
  "Improved PEP access",
  "Improved PEP + mod MDV",
  "Improved PEP + MDV"
)

create_temporal_plot(df_deaths, title="Deaths", scenarios= scenario_order2)


## Costs and deaths averted #####

params2 <- read.csv("./data/epidemics_params2.csv") 

CE_results  <- run_country(params2,  "Tanzania") 

# summarise across scnerios
CE_summary <- summarise_across_scenarios(
  country_results = CE_results,
  matrix_names = c("ts_deaths", "ts_deaths_averted", "ts_MDV_campaign_cost", "ts_start_PEP")
) %>%
  group_by(scenario)

summarised_results <- map(CE_summary, ~ {
  .x %>%
    group_by(scenario) %>%
    summarise(total_median = sum(Median, na.rm = TRUE), .groups = "drop")
})

summarised_results <- imap(summarised_results, ~ {
  rename(.x, !!paste0(.y, "") := total_median)
})

summarised_results <- reduce(summarised_results, full_join, by = "scenario") %>%
  dplyr::mutate(
    PEP_cost = case_when(
      scenario %in% c("SQ", "imp_access_IM") ~ ts_start_PEP * 4 * 10,
      TRUE                                   ~ ts_start_PEP * 2.4 * 10
    ),
    total_cost = PEP_cost+ ts_MDV_campaign_cost,
    inc_cost  = total_cost - total_cost[scenario == "SQ"],
    inc_deaths_averted = ts_deaths_averted - ts_deaths_averted[scenario == "SQ"]
  )

summarised_results


# ICER plot

ce_data <- summarised_results %>%
  # Compare alternatives vs SQ (drop baseline from the plot)
  dplyr::filter(scenario != "SQ") %>%
  dplyr::mutate(
    inc_cost_m = inc_cost / 1e6   # nicer scale for y-axis
  )
ce_data_with_SQ <- bind_rows(
  tibble(scenario = "SQ", inc_deaths_averted = 0, inc_cost_m = 0),
  ce_data
) %>%
  dplyr::mutate(scenario = dplyr::recode(
    scenario,
    SQ = "Status Quo",
    imp_access_ID = "Improved ID PEP access",
    imp_access_IM = "Improved IM PEP access",
    imp_access_modMDV = "Improved PEP + mod MDV",
    imp_access_MDV = "Improved PEP + MDV"
  )) 

x_max <- max(ce_data$inc_deaths_averted, na.rm = TRUE)

ggplot(ce_data_with_SQ, aes(x = inc_deaths_averted, y = inc_cost_m)) +
  # reference lines
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.6) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.6) +
  
  # points
  geom_point(size = 3, colour = "orchid4") +
  
  # labels that don't get cut off
  ggrepel::geom_text_repel(aes(label = scenario),
                           size = 3,
                           min.segment.length = 0,
                           box.padding = 0.2,
                           point.padding = 0.2) +
  
  scale_x_continuous(
    limits = c(0, x_max * 1.1),       # ensure all x > 0 and space for labels
    expand = expansion(mult = c(0, 0.05)),
    name   = "Incremental deaths averted"
  ) +
  scale_y_continuous(
    name = "Incremental cost(USD M)"
  ) +
  coord_cartesian(clip = "off") +      # allow labels beyond plotting area
  theme_minimal() +
  theme(
    plot.margin = margin(5.5, 40, 5.5, 5.5),  # extra space on right for labels
    panel.grid.minor = element_blank()
  )







