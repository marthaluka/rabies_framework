


# 1. Using IBCM data ########
## Read IBCM data ########

ibcm_bites_mon <- read.csv("./data/ibcm_monthly_bites.csv") %>%
  dplyr::select(-c(X)) %>%
  dplyr::mutate(Year_month = as.Date(Year_month),
                year = year(Year_month))

ibcm_bites_year <- ibcm_bites_mon %>%
  group_by(District_facility, year) %>%
  summarise(annual_bites = sum(total_n)) %>%
  dplyr::filter(year > 2018)
  
ibcm_bites_year_summ <- ibcm_bites_year %>%
  summarise(mean_annual_bites = mean(annual_bites),
            upper_limit = max(annual_bites),
            lower_limit = min(annual_bites))


## Run model, district by district ########
 # Here, we assume the status quo ie no vax efforts

## read district pop and hdrs
district_data <- read.csv("./data/ibcm_district_hdrs_maganga.csv") %>%
  dplyr::select(District, lower_limit, upper_limit, population)

## Initialize an empty list to store results
district_results <- list()

## Loop through each district and run the model
for (i in 1:nrow(district_data)) {
  district <- district_data$District[i]
  pop <- district_data$population[i]
  hdr_range <- c(district_data$lower_limit[i], district_data$upper_limit[i])
  
  # Run the model
  result <- decision_tree(
    N = 100,
    pop = pop,
    HDR = hdr_range,
    horizon = 5,
    mu = 0.38,
    k = 0.14,
    pBite_healthy = 0.01,
    pSeek_healthy = 0.6,
    pStart_healthy = 0.2,
    pComplete_healthy = 0.2,
    pSeek_exposure = 0.7,
    pStart_exposure = 0.666667,
    pComplete_exposure = 0.3968,
    pDeath = 0.17,
    pPrevent = 0.986,
    full_cost = 45,
    partial_cost = 25,
    mdv_campaign_budget = NULL,
    base_vax_cov = 0.05,
    vaccinate_dog_cost = c(2,4),
    target_vax_cov = 0,
    pInvestigate = 0.5,
    pFound = 0.4,
    pTestable = 0.2,
    pFP = 0.05
  )
  
  # Save the result under the district name
  district_results[[district]] <- result
}



summarise_wrapper <- function(matrix_name){
  summary_list <- lapply(names(district_results), function(district) {
    mat <- district_results[[district]][[matrix_name]]
    out <- summarise_stochasticity(mat)
    out$scenario <- district  # Add scenario manually
    return(out)
  })
  
  summary_df <- do.call(rbind, summary_list)
  return(summary_df)
}



model_exp_seek_care <- summarise_wrapper("ts_exp_seek_care")%>%
  dplyr::rename(District_facility = scenario)


# Compare with data


ibcm_bites_year_summ2 <- ibcm_bites_year_summ %>%
  dplyr::filter(District_facility %in% names(district_results)) 

ggplot() +
  geom_line(data = model_exp_seek_care, aes(x=year, y=Median), color = "purple") +
  geom_ribbon(data = model_exp_seek_care, aes(x = year, ymin = LL, ymax = UL), fill = "purple", alpha = 0.5, color = NA) +
  geom_point(data = ibcm_bites_year_summ2, aes(x = 2.5, y = mean_annual_bites)) +
  geom_errorbar(data = ibcm_bites_year_summ2, aes(x = 2.5, ymin = lower_limit, ymax = upper_limit))+
  facet_wrap(~District_facility, scales = "free_y")+
  theme_minimal()





# 1. Using Pemba data ########