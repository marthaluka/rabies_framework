

# model
source("./scripts/stochastic_decision_tree.R")

# read parameter file
country_params <- read.csv("./data/Gavi_country_simulatiion.csv", header=T)

head(country_params)

run_decision_tree_and_select_variables2 <- function(scenario_name, parameters_df, horizon = 2, base_vax_cov=0.05, 
                                                   N = 100, selected_vars=NULL){
  scenario_parameters <- parameters_df[parameters_df$scenario == scenario_name, ]
  
  result <- decision_tree(
    N = N,
    pop = scenario_parameters$population,
    horizon = horizon, 
    base_vax_cov=base_vax_cov,
    discount = scenario_parameters$discount,
    target_vax_cov = scenario_parameters$target_vax_cov,
    HDR = c(scenario_parameters$HDR1, scenario_parameters$HDR2),
    pBite_healthy = scenario_parameters$pBite_healthy,
    mu = scenario_parameters$mu,
    k = scenario_parameters$k,
    pSeek_healthy = scenario_parameters$pSeek_healthy,
    pStart_healthy = scenario_parameters$pStart_healthy,
    pComplete_healthy = scenario_parameters$pComplete_healthy,
    pSeek_exposure = scenario_parameters$pSeek_exposure,
    pStart_exposure = scenario_parameters$pStart_exposure,
    pComplete_exposure = scenario_parameters$pComplete_exposure,
    pDeath = scenario_parameters$pDeath,
    pPrevent = scenario_parameters$pPrevent,
    full_cost = scenario_parameters$full_cost,
    partial_cost = scenario_parameters$partial_cost,
    vaccinate_dog_cost = c(scenario_parameters$vaccinate_dog_cost1, scenario_parameters$vaccinate_dog_cost2),
    pInvestigate = scenario_parameters$pInvestigate,
    pFound = scenario_parameters$pFound,
    pTestable = scenario_parameters$pTestable,
    pFN = scenario_parameters$pFalseNeg
  )
  
  # If specific variables are provided, subset the output to only those variables
  if(!is.null(selected_vars) && is.vector(selected_vars)){
    result <- result[selected_vars]
  }
  
  return(result)
}


# find upper limit, median and lower limit from list of length N
summarise_stochasticity2 <-  function(mat_list, matrix_name){
  # Check if input is a list
  if(!is.list(mat_list)) stop("Input should be a list of matrices")
  
  results_list <- lapply(seq_along(mat_list), function(idx) {
    sublist <- mat_list[[idx]]
    scenario_name <- names(mat_list)[idx]  # extract the scenario name (e.g., 'mdv_0')
    # Check if the sublist contains the specified matrix
    if(!is.null(sublist[[matrix_name]])) {
      mat <- sublist[[matrix_name]]
      out <- apply(mat, 2, quantile, c(0.025, 0.5, 0.975), na.rm=TRUE)
      df <- as.data.frame(t(out))
      names(df) <- c('LL', 'Median', 'UL')
      df$year <- 1:dim(mat)[2]   # assign the column number to "year"
      df$scenario <- scenario_name  # add the scenario name column
      return(df)
    }
    return(NULL)
  })
  
  # Filter out NULLs and combine the results into a single dataframe
  results_list <- Filter(Negate(is.null), results_list)
  combined_results <- do.call(rbind, results_list)
  return(combined_results)
}




## run scenarios
country_scenarios <- country_params$scenario

output <- lapply(country_scenarios, run_decision_tree_and_select_variables2, 
                     parameters_df=country_params, 
                     selected_vars = c('ts_healthy_start', 'ts_exp_start', 
                                       'ts_rabid_dogs', 'ts_exposures', 'ts_healthy_bites')
                 )


names(output) <- country_scenarios


mean(output$Tanzania_2024$ts_exp_start[,1])
median(output$Tanzania_2024$ts_exp_start[,1])

mean(output$Tanzania_2024$ts_healthy_start[,1])
median(output$Tanzania_2024$ts_healthy_start[,1])


mean(output$Tanzania_2024$ts_rabid_dogs[,1])

# summarise

# our dict
my_dict<- country_params  %>%
  dplyr::select(scenario, country, year)

out1<- summarise_stochasticity2(output, 'ts_healthy_start')  %>%
  dplyr::filter(year==1)  %>%
  dplyr::select(-c(year)) %>%
  left_join(., my_dict, by="scenario")


out2<- summarise_stochasticity2(output, 'ts_exp_start')  %>%
  dplyr::filter(year==1)  %>%
  dplyr::select(-c(year)) %>%
  left_join(., my_dict, by="scenario")


total_out <- bind_rows(out1, out2) %>%
  group_by(scenario) %>%
  summarize(
    LL = sum(LL, na.rm = TRUE),
    Median = sum(Median, na.rm = TRUE),
    UL = sum(UL, na.rm = TRUE),
    country = first(country),
    year = first(year)
  )


#write to file

write.csv(total_out, "./figures/pts_start_PEP.csv")




