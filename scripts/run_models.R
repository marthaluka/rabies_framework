

# libraries

require(pacman)
pacman::p_load(tidyverse, # cleaning, wrangling
               scales,    # display neat number values
               paletteer,  # cool color palettes
               brms
               )

# source model
source("./scripts/decision_tree_wrapper.R")


## read parameters file
parameters_df <- read.csv("./data/country_params_MLedited.csv") %>%
  dplyr::mutate(
    pop = readr::parse_number(as.character(pop)),
    bpi_per100k = readr::parse_number(as.character(bpi_per100k))
  )

parameters_df %>%
  dplyr::select(country, Administrative_unit, scenario)



# Common params
common_params <- list(
    baseline_surveillance = 0.01,
    N = 1000, horizon = 10, discount = 0.03, seed = 123,
    mu = 0.38, k = 0.72, rabies_inc = c(0.0075, 0.0125),
    pDeath = 0.17, pPrevent_complete = 0.999, pPrevent_incomplete = 0.986,
    mdv_unowned_budget = NULL, mdv_owned_budget = NULL, years_to_target = 3, dog_burnin = 3,
    ibcm_increase_exposure_care_seeking = TRUE, ibcm_increase_exposure_compliance = TRUE,
    ibcm_reactive_vaccination = TRUE #ibcm_decrease_healthy_compliance = TRUE, 
)


# Convert CSV to model arguments
make_model_args <- function(pars, common_params) {
  
  stopifnot(nrow(pars) == 1)
  
  # Parameters taken from CSV
  csv_params <- list(
    pop = pars$pop, 
    HDR = c(pars$HDR1, pars$HDR2),
    unowned_prop = pars$unowned_prop, 
    bpi = pars$bpi_per100k,
    pStart_healthy = pars$pStart_healthy,
    pCompliance_healthy = pars$pCompliance_healthy,
    pSeek_exposure = pars$pSeek_exposure,
    pStart_exposure = pars$pStart_exposure,
    pCompliance_exp = pars$pCompliance_exp,
    vaccinate_owned_dog_cost = c(pars$vaccinate_owned_dog_cost1, pars$vaccinate_owned_dog_cost2),
    vaccinate_unowned_dog_cost = c(pars$vaccinate_unowned_dog_cost1,pars$vaccinate_unowned_dog_cost2),
    base_vax_cov_owned = pars$base_vax_cov_owned,
    target_vax_cov_owned = pars$target_vax_cov_owned,
    base_vax_cov_unowned = pars$base_vax_cov_unowned,
    target_vax_cov_unowned = pars$target_vax_cov_unowned,
    RIG_cov = pars$RIG_cov,
    PEP_vials_per_pt = pars$PEP_vials_per_pt,
    human_vaccine_cost_per_vial = pars$human_vaccine_cost_per_vial,
    RIG_cost = pars$RIG_cost,
    ibcm = pars$ibcm,
    pInvestigate = pars$pInvestigate,
    pFound = pars$pFound,
    pTestable = pars$pTestable,
    pFS = pars$pFS,
    dogs_per_reactive_vax = pars$dogs_per_reactive_vax,
    max_pSeek_exposure = pars$max_pSeek_exposure,
    max_pCompliance_exp = pars$max_pCompliance_exp,
    #min_pCompliance_healthy = pars$min_pCompliance_healthy,
    max_dog_vax_cov = pars$max_dog_vax_cov,
    max_reactive_vaccinations = pars$max_reactive_vaccinations,
    reactive_vax_cost_per_dog = pars$reactive_vax_cost_per_dog,
    investigation_cost_per_dog = pars$investigation_cost_per_dog,
    dog_test_cost_per_dog = pars$dog_test_cost_per_dog
  )
  
  # CSV parameters override common parameters
  args <- utils::modifyList(common_params,csv_params)
  
  return(args)
}


args <- make_model_args(pars = parameters_df[1, ],common_params = common_params)

names(args)

# Run one country × scenario
run_model <- function(pars) {
  
  args <- make_model_args(
    pars = pars,
    common_params = common_params
  )
  
  do.call(decision_tree_wrapper, args)
}

# Parallel setup 
future::plan(
  future::multisession,
  workers = 8
)

# Run all country × scenario combinations 
# Always load the models 
load_rabies_models()


# Name the results to match parameter table

run_names <- with(
  parameters_df,
  paste(country, Administrative_unit, scenario, sep = "_")
)

results <- furrr::future_map(
  seq_len(nrow(parameters_df)),
  function(i) {
    run_model(parameters_df[i, , drop = FALSE])
  },
  .options = furrr::furrr_options(seed = TRUE),
  .progress = TRUE
) %>%
  set_names(run_names)



# Save rds

saveRDS(results, "output/country_results.rds")




# results <- readRDS("output/country_results.rds")
















# find upper limit, median and lower limit from list of length N
summarise_stochasticity <-  function(my_matrix){
  out<- apply(my_matrix, 2, quantile, c(0.025, 0.5, 0.975), na.rm=TRUE)
  rownames(out)<-NULL
  return(out)
}

# select variable from an object/scenario
select_variable <- function(variable, scenario){
  my_matrix <- scenario[[variable]]
  out<- summarise_stochasticity(my_matrix)
  df <- as.data.frame(t(out))
  names(df) <- c('LL', 'Median', 'UL')
  return(df)
}



# return time series values 
df <- select_variable(variable='ts_rabid_dogs', scenario=MDV_only)
# run this across all variables and store the summarised object instead

# Plotting to check
ggplot(df, aes(x = as.numeric(row.names(df)), y = Median)) +
  geom_line() +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  ylab("ts_rabid_dogs")+ xlab("Year")+
  theme_bw() 


# Compare >2 scenarios (using cumulative values over horizon ie instead of yearly)
compare_scenarios <- function(variable, ...){
  # Get the list of scenarios from arguments
  scenarios <- list(...)
  scenario_names <- as.character(substitute(list(...)))[-1]  # Capture the names of the scenarios
  
  # Process each scenario
  compare_df_list <- mapply(function(scenario, name) {
    df <- colSums(select_variable(variable, scenario))
    df$scenario <- name  # Add the scenario name as a new column
    return(df)
  }, scenario = scenarios, name = scenario_names, SIMPLIFY = FALSE)
  
  # Combine the results into one dataframe
  compare_df <- as.data.frame(do.call("rbind", compare_df_list))
  
  # Convert list columns into a more manageable data type
  for (col_name in names(compare_df)) {
    if (is.list(compare_df[[col_name]])) {
      compare_df[[col_name]] <- as.character(unlist(compare_df[[col_name]]))
    }
  }
  
  return(compare_df)
  
}

compare_scenarios("ts_cost_PEP_per_year", PEP_IM_free_only, PEP_ID_free_only)
compare_scenarios("ts_cost_PEP_per_year", PEP_IM_free_only, PEP_ID_free_only)

# Deaths averted by MDV (cumulative over horizon)
deaths_df <- compare_scenarios("ts_deaths", no_interventions, MDV_only) %>%
  dplyr::mutate(across(c(LL, Median, UL), as.numeric))

deaths_averted_MDV <- deaths_df[deaths_df$scenario == "no_interventions", ][,1:3] - deaths_df[deaths_df$scenario == "MDV_only", ][,1:3] 


# PEP COSTS #####
# Plot costs under different policy choice across horizon
pep_costs <- compare_scenarios("ts_cost_PEP_per_year", PEP_IM_free_only, PEP_ID_free_only) %>%
  as.data.frame() %>%
  dplyr::mutate(scenario = case_when(
    scenario == "PEP_IM_free_only" ~ "Intramuscular",
    scenario == "PEP_ID_free_only" ~ "Intradermal")
  )
  

# convert columns to numeric
pep_costs$LL <- as.numeric(pep_costs$LL)
pep_costs$Median <- as.numeric(pep_costs$Median)
pep_costs$UL <- as.numeric(pep_costs$UL)


ggplot(pep_costs, aes(x=scenario, y=Median, color=scenario)) +
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LL, ymax=UL), width=.2,
                position=position_dodge(0.05)) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values=c('#999999', '#E69F00'))+
  labs(title = "Estimated national costs of PEP",
       y = "US dollars")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle=0, color="black", size=13),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.y = element_text(color="black", size=13),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) 


# Compare deaths &/ lives saved across policy choices #####

cumulative_deaths <- compare_scenarios("ts_deaths", no_interventions, PEP_ID_free_only, MDV_only, MDV_PEP_ID_free) %>%
  as.data.frame() %>%
  dplyr::mutate(scenario = case_when(
    scenario == "no_interventions" ~ "Status quo",
    scenario == "PEP_ID_free_only" ~ "free PEP",
    scenario == "MDV_only" ~ "MDV",
    scenario == "MDV_PEP_ID_free" ~ "MDV and free PEP")
  )

# convert columns to numeric
cumulative_deaths$LL <- as.numeric(cumulative_deaths$LL)
cumulative_deaths$Median <- as.numeric(cumulative_deaths$Median)
cumulative_deaths$UL <- as.numeric(cumulative_deaths$UL)

# Reorder the levels of scenario factor
cumulative_deaths <- cumulative_deaths %>%
  mutate(scenario = factor(scenario, levels = unique(scenario[order(-Median)])))


ggplot(cumulative_deaths, aes(x=scenario, y=Median, color=scenario)) +
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LL, ymax=UL), width=.2,
                position=position_dodge(0.05)) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = grDevices::rgb(1,0,0, alpha = c(1, 0.8, 0.6, 0.4))) +
  #scale_color_paletteer_d("colorBlindness::Blue2DarkRed12Steps") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle=0, color="black", size=13),
    axis.title.y = element_blank(),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.text.y = element_text(color="black", size=13),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  coord_flip() +
  labs(title = "Impact of policy choice",
       y = "Human deaths")





# plot

# plot_slope_chart <- function(data) {
#   # Ensure data is ordered by scenario
#   #data <- data[order(data$scenario), ]
#
#   # Define the color palette
#   colors <- paletteer_d("beyonce::X6")
#
#   ggplot(my_data, aes(x = scenario, y = Median, group = 1)) +
#     geom_line(color = "grey", size = 0.5) +
#     geom_point(aes(y = Median, color = scenario), size = 3) #+
#     # #geom_crossbar(aes(ymin = LL, ymax = UL, color = scenario), width = 0.2, size = 0.5) +
#     # geom_linerange(aes(ymin = LL, ymax = UL, color = scenario), size = 0.5) +
#     # labs(y = "Value", x = "Scenario") +
#     # #theme_minimal() +
#     # #scale_color_manual(values = colors, name = "Scenario") +
#     # scale_y_continuous(labels = comma)+
#     # theme(legend.position = "none")
# }
#
# plot_slope_chart(my_data)
#
#
# as.factor(unlist(my_data$scenario))
#
#
# data <- my_data$LL


# Create a sample data frame
data <- data.frame(
  Policy = c("Exp_start_PEP", "Exp_complete_PEP", "Deaths"),
  PEP_free = c(53, 45, 2),
  Status_quo = c(31, 12, 6)
)

# Melt data into long format
library(reshape2)
data_long <- melt(data, id.vars="Policy")


# Create the slope chart with labels for 'Status quo' and legend
ggplot(data_long, aes(x=variable, y=value, group=Policy)) + 
  geom_line(aes(color=Policy), size=1) + 
  geom_point(aes(color=Policy), size=3) +
  geom_text(aes(label=value), hjust=-0.2, vjust=0) +
  geom_text(aes(label=ifelse(variable == 'Status_quo', Policy, NA), color=Policy),
            hjust=-0.2, vjust=0) +  # Point labels
  labs(
    title="Policy choices for PEP",
    x=NULL,
    y=NULL
  ) +
  theme_minimal() +
  theme(
    legend.position="none",
    axis.text.x=element_text(angle=0, hjust=0.5)
  )










