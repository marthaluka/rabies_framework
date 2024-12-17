

# libraries
require(pacman)
pacman::p_load(tidyverse, # cleaning, wrangling
               scales,    # display neat number values
               paletteer,  # cool color palettes
               viridis,    #colours
               patchwork,   # merge plots
               plotly,    # 3D plots
               brms
)

# source model
source("./scripts/stochastic_decision_tree.R")



# extract parameter values from csv
run_decision_tree_and_select_variables <- function(scenario_name, parameters_df, pop=60000000, horizon = 7, base_vax_cov=0.05, 
                                                   N = 1000, selected_vars=NULL){
  scenario_parameters <- parameters_df[parameters_df$scenario == scenario_name, ]
  
  result <- decision_tree(
    N = N,
    pop = pop,
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


# Plots ########
## Plot 1 ########
create_temporal_plot <- function(mydata, title, scenarios){
  # order scenarios to logic rather than alphanumeric
  mydata$scenario <- factor(mydata$scenario, levels = scenarios)
  #plot
  ggplot(mydata, aes(x = year, y = Median, group = scenario, color = scenario, fill = scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.5) +
  facet_wrap(~scenario) +
  labs(
    title = paste(title, "with 95% Confidence Intervals"),
    x = "Year",
    y = title
  ) +
    theme_minimal()+ 
    scale_y_continuous(labels = scales::comma) +
    theme(
      legend.position = "none"
    )
}


## Plot 2 ########
create_timepoint_plot <-function(mydata, title, scenarios){
  # Interested in time points 3, 5 and 7 years
  filtered_data <- mydata[mydata$year %in% c(3, 5, 7), ]

  # Calculate the sum of UL, LL, and Median for "total"
  total_data <- aggregate(. ~ scenario, data = mydata, FUN = sum)
  # rbind
  plot_data <- rbind(filtered_data, total_data) %>%
    dplyr::mutate(year = ifelse(year == 28, "total", as.character(year)))
  # Manually specify the order of years
  year_order <- c("total", "3", "5", "7")
  
  # order scenarios to logic rather than alphanumeric
  plot_data$scenario <- factor(plot_data$scenario, levels = scenarios)

  # # Create the visualization
  total_data$scenario <- factor(total_data$scenario, levels = scenarios)
  ggplot(total_data, aes(x = scenario, y = Median, group = scenario, color = scenario)) +
      geom_point() +
      geom_errorbar(aes(ymin = LL, ymax = UL), width = 0.2) +
      labs(
        #title = paste("Rabies", title, "with 95% Confidence Intervals"),
        x = "Scenario",
        y = paste("Cumulative", title)
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "none"
                  )

  # total, years 3,5 & 7 
  # ggplot(plot_data, aes(x = factor(year, levels = year_order), y = Median, group = scenario, color = scenario)) +
  #   geom_point() + 
  #   geom_errorbar(aes(ymin = LL, ymax = UL), width = 0.2) +
  #   labs(
  #     title = paste("Rabies", title, "with 95% Confidence Intervals"),
  #     x = "Year",
  #     y = title
  #   ) +
  #   theme_minimal() +
  #   scale_y_continuous(labels = scales::comma) +
  #   facet_wrap(~scenario) +
  #   scale_color_manual(values = viridis::viridis_pal()(length(scenarios)))+
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #         legend.position = "none"
  #         )
}



summarise_and_plot_2D <- function(mat_list, matrix_name, title, scenarios){
  mydata <- summarise_stochasticity2(mat_list, matrix_name)
  plot_a <- create_temporal_plot(mydata, title, scenarios)
  plot_b <- create_timepoint_plot(mydata, title, scenarios)
  
  out_plot <- plot_a + plot_b
  return(out_plot)
}
  

#  MDV Sensitivity######
## read file
mdv_range <- read.csv("./data/sensitivity_params/mdv_sensitivity.csv")

## run scenarios
mdv_scenarios <- mdv_range$scenario

mdv_output <- lapply(mdv_scenarios, run_decision_tree_and_select_variables, 
                     parameters_df=mdv_range, 
                     selected_vars = c('ts_rabid_dogs', 'ts_exposures', 'ts_healthy_bites', 'ts_deaths', 
                                       'ts_exposures_seek_care_inc', 'ts_total_seek_care_inc')
                     )

names(mdv_output) <- mdv_scenarios

names(mdv_output)

mdv_output$mdv0$ts_rabid_dogs

# # Summarise stochasticicty
#   ## data frames
# deaths_df <- summarise_stochasticity2(mdv_output, 'ts_deaths')
# testable_df <- summarise_stochasticity2(mdv_output, 'ts_rabid_biting_testable')
# 
# # Plots
# deaths_temporal_plot <- create_temporal_plot(deaths_df, "deaths", scenarios=mdv_scenarios) ; deaths_plot
# deaths_tms_plot <- create_timepoint_plot(deaths_df, "deaths", scenarios = mdv_scenarios) ; deaths_tms_plot

mdv1<- summarise_and_plot_2D(mdv_output, 'ts_rabid_dogs', 'Rabid dogs', mdv_scenarios) ; mdv1
mdv2<- summarise_and_plot_2D(mdv_output, 'ts_exposures', 'Exposures', mdv_scenarios) ; mdv2
mdv3<- summarise_and_plot_2D(mdv_output, 'ts_healthy_bites', 'Healthy bites', mdv_scenarios) ; mdv3
mdv4<- summarise_and_plot_2D(mdv_output, 'ts_deaths', 'Deaths', mdv_scenarios) ; mdv4
mdv5<- summarise_and_plot_2D(mdv_output, 'ts_exposures_seek_care_inc', 'Exposures seek care inc', mdv_scenarios) ; mdv5
mdv6<- summarise_and_plot_2D(mdv_output, 'ts_total_seek_care_inc', 'All seek care inc', mdv_scenarios) ; mdv6

# HDR sensistivity ######
    # Note: pop control may be useful at some point?
hdr_range <- read.csv("./data/sensitivity_params/hdr_sensitivity.csv")

## run scenarios
hdr_scenarios <- hdr_range$scenario

hdr_output <- lapply(hdr_scenarios, run_decision_tree_and_select_variables, 
                     parameters_df=hdr_range, 
                     selected_vars = c('ts_rabid_dogs', 'ts_exposures', 'ts_healthy_bites', 'ts_deaths', 'ts_total_seek_care_inc')
                     )
names(hdr_output) <- hdr_scenarios

#plot
hdr1<- summarise_and_plot_2D(hdr_output, 'ts_rabid_dogs', 'Rabid dogs', hdr_scenarios) ; hdr1
hdr2<- summarise_and_plot_2D(hdr_output, 'ts_exposures', 'Exposures', hdr_scenarios) ; hdr2
hdr3<- summarise_and_plot_2D(hdr_output, 'ts_healthy_bites', 'Healthy bites', hdr_scenarios) ; hdr3
hdr4<- summarise_and_plot_2D(hdr_output, 'ts_deaths', 'Deaths', hdr_scenarios) ; hdr4
hdr5<- summarise_and_plot_2D(hdr_output, 'ts_total_seek_care_inc', 'All seek care inc', hdr_scenarios) ; hdr5

hdr3/hdr4

(hdr1 + hdr2)/ (hdr4 + hdr5)

# MDV HDR Sensitivity ##########
    # Note: cost of mdv may be too high at some point (extremely high dog pop)!?
        #Even with 100% vax cov, there will be incursions

## read file
mdvHDR_range <- read.csv("./data/sensitivity_params/mdv_hdr_sensitivity.csv")

## run scenarios
mdvHDR_scenarios <- mdvHDR_range$scenario

mdvHDR_output <- lapply(mdvHDR_scenarios, run_decision_tree_and_select_variables, 
                     parameters_df=mdvHDR_range, N = 100,
                     selected_vars = c('ts_rabid_dogs', 'ts_exposures', 'ts_deaths', 
                                       'ts_exposures_seek_care_inc', 'ts_total_seek_care_inc')
                     )

names(mdvHDR_output) <- mdvHDR_scenarios


process_data <- function(mat_list, mat_name, col1, col2) {
  # Separate the 'scenario' column into 'mdv' and 'hdr'
  data <- summarise_stochasticity2(mat_list, mat_name) 
  
  # Calculate the sum of UL, LL, and Median for "total"
  total_data <- data %>%
    group_by(scenario) %>%
    dplyr::summarise(across(where(is.numeric), sum)) %>%
    separate(scenario, into = c(col1, col2), sep = "_", remove = FALSE) %>%
    dplyr::select(-c(year))
  
  total_data[[col1]] <- as.numeric(gsub(col1, "", total_data[[col1]]))
  total_data[[col2]] <- as.numeric(gsub(col2, "", total_data[[col2]]))
  
  return(total_data)
}

a<-process_data(mat_list=mdvHDR_output, mat_name='ts_exposures', col1='mdv', col2='hdr')

# Create a 3D  plot
create_3d_scatter_plot <- function(data, x, y, z, title) {
  plot_ly(
    data = data,
    x = ~data[[x]],
    y = ~data[[y]],
    z = ~data[[z]],
    type = "scatter3d",
    mode = "markers",
    marker = list(
      color = ~data[[z]],
      #colorscale = c("blue", "green", "red"),
      cmin = min(data[[z]]),
      cmax = max(data[[z]])
    )
  ) %>%
    layout(
      scene = list(
        xaxis = list(title = x),
        yaxis = list(title = y),
        zaxis = list(title = z)
      ),
      title = paste("Cumulative", title)
    )
}

create_3d_scatter_plot(a, 'hdr', 'mdv', 'Median', "Exposures")

process_and_plot_3D <- function(mat_list, mat_name, col1, col2, z = 'Median', title){
  
  mydata<-process_data(mat_list=mat_list, mat_name=mat_name, col1=col1, col2=col2)
  myplot <- create_3d_scatter_plot(data = mydata, x=col1, y=col2, z=z, title=title)
  
  return(myplot)
  
}


# plot
mdvHDR1<- process_and_plot_3D(mat_list=mdvHDR_output, mat_name=ts_rabid_dogs, col1='mdv', col2='hdr', z = 'Median', title="Rabid dogs"); mdvHDR1
mdvHDR2<- process_and_plot_3D(mat_list=mdvHDR_output, mat_name=ts_exposures, col1='mdv', col2='hdr', z = 'Median', title="Exposures"); mdvHDR2
mdvHDR3<- process_and_plot_3D(mat_list=mdvHDR_output, mat_name=ts_healthy_bites, col1='mdv', col2='hdr', z = 'Median', title="Healthy bites"); mdvHDR3
mdvHDR4<- process_and_plot_3D(mat_list=mdvHDR_output, mat_name=ts_deaths, col1='mdv', col2='hdr', z = 'Median', title="Deaths"); mdvHDR4
mdvHDR5<- process_and_plot_3D(mat_list=mdvHDR_output, mat_name=ts_exposures_seek_care_inc, col1='mdv', col2='hdr', z = 'Median', title="Exposures seek care incidence"); mdvHDR5
mdvHDR6<- process_and_plot_3D(mat_list=mdvHDR_output, mat_name=ts_total_seek_care_inc, col1='mdv', col2='hdr', z = 'Median', title="Total seek care"); mdvHDR6
mdvHDR7<- process_and_plot_3D(mat_list=mdvHDR_output, mat_name=ts_MDV_campaign_cost, col1='mdv', col2='hdr', z = 'Median', title="MDV campaign cost"); mdvHDR7





# Health seeking #######
## pSeek exposures ######
pSeek_range <- read.csv("./data/sensitivity_params/pSeek_exposures.csv")

## run scenarios
pSeek_scenarios <- pSeek_range$scenario

pSeek_output <- lapply(pSeek_scenarios, run_decision_tree_and_select_variables, 
                     parameters_df=pSeek_range, 
                     selected_vars = c('ts_exposures_seek_care', 'ts_deaths_averted_PEP',  'ts_deaths')
                     )
names(pSeek_output) <- pSeek_scenarios

#plot
pSeek1<- summarise_and_plot_2D(pSeek_output, 'ts_exposures_seek_care', 'Exposures seek care', pSeek_scenarios) ; pSeek1
pSeek2<- summarise_and_plot_2D(pSeek_output, 'ts_deaths_averted_PEP', 'Deaths averted PEP', pSeek_scenarios) ; pSeek2
pSeek3<- summarise_and_plot_2D(pSeek_output, 'ts_deaths', 'Deaths', pSeek_scenarios) ; pSeek3

pSeek1/pSeek2/pSeek3

## pStart exposures ######
pStart_range <- read.csv("./data/sensitivity_params/pStart_exposures.csv")

## run scenarios
pStart_scenarios <- pStart_range$scenario

pStart_output <- lapply(pStart_scenarios, run_decision_tree_and_select_variables, 
                       parameters_df=pStart_range, 
                       selected_vars = c('ts_exp_start', 'ts_deaths_averted_PEP',  'ts_deaths')
)
names(pStart_output) <- pStart_scenarios

#plot
pStart1<- summarise_and_plot_2D(pStart_output, 'ts_exp_start', 'Exposures start PEP', pStart_scenarios) ; pStart1
pStart2<- summarise_and_plot_2D(pStart_output, 'ts_deaths_averted_PEP', 'Deaths averted PEP', pStart_scenarios) ; pStart2
pStart3<- summarise_and_plot_2D(pStart_output, 'ts_deaths', 'Deaths', pStart_scenarios) ; pStart3
 

pStart1/pStart2/pStart3   

## pComplete exposures ######
    # Note: other factors not in model may affect pStart
pComplete_range <- read.csv("./data/sensitivity_params/pComplete_exposures.csv")

## run scenarios
pComplete_scenarios <- pComplete_range$scenario

pComplete_output <- lapply(pComplete_scenarios, run_decision_tree_and_select_variables, 
                        parameters_df=pComplete_range, 
                        selected_vars = c('ts_exp_complete', 'ts_deaths_averted_PEP',  'ts_deaths')
                        )
names(pComplete_output) <- pComplete_scenarios

#plot
pComplete1<- summarise_and_plot_2D(pComplete_output, 'ts_exp_complete', 'Exposures complete PEP', pComplete_scenarios) ; pComplete1
pComplete2<- summarise_and_plot_2D(pComplete_output, 'ts_deaths_averted_PEP', 'Deaths averted PEP', pComplete_scenarios) ; pComplete2
pComplete3<- summarise_and_plot_2D(pComplete_output, 'ts_deaths', 'Deaths', pComplete_scenarios) ; pComplete3

pComplete1/pComplete2/pComplete3

# Health seeking exposures combined #######

health_seeking_range <- read.csv("./data/sensitivity_params/healthSeeking_combined_ed.csv")

## run scenarios
health_seeking_scenarios <- health_seeking_range$scenario

health_seeking_output <- lapply(health_seeking_scenarios, run_decision_tree_and_select_variables, 
                        parameters_df=health_seeking_range, N=100,
                        selected_vars = c('ts_exp_complete', 'ts_deaths', 'ts_deaths_averted_PEP')
                        )

names(health_seeking_output) <- health_seeking_scenarios

# process_data_4D
process_data_4D <- function(mat_list, mat_name, col1, col2,col3) {
  # Separate the 'scenario' column into 'mdv' and 'hdr'
  data <- summarise_stochasticity2(mat_list, mat_name) 
  
  # Calculate the sum of UL, LL, and Median for "total"
  total_data <- data %>%
    group_by(scenario) %>%
    dplyr::summarise(across(where(is.numeric), sum)) %>%
    separate(scenario, into = c(col1, col2, col3), sep = "_", remove = FALSE) %>%
    dplyr::select(-c(year))
  
  total_data[[col1]] <- as.numeric(gsub(col1, "", total_data[[col1]]))
  total_data[[col2]] <- as.numeric(gsub(col2, "", total_data[[col2]]))
  total_data[[col3]] <- as.numeric(gsub(col3, "", total_data[[col3]]))
  
  return(total_data)
}

mydata<-process_data_4D(mat_list=health_seeking_output, mat_name='ts_deaths_averted_PEP', col1='pStart', col2='pComplete', col3='pSeek')

mydata2 <- mydata %>%
  dplyr::filter(pComplete==1)

# plot
create_3d_scatter_plot(data=mydata2, x='pStart', y='pSeek', z='Median', title='ts_deaths_averted_PEP')

# View the plots
plots

# All?? ##########







