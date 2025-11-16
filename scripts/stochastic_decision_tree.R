

require(pacman)
pacman::p_load(tidyverse,   # cleaning, wrangling
               brms,      # models
               rlang,
               lubridate
               )



source("./scripts/HelperFun.R")




#########
# run

decision_tree <- function(N = 10, pop = 100000, HDR = c(10,20), horizon = 5, discount = 0.03, mu = 0.38, k = 0.14, 
                          pSeek_healthy = 0.2, pBite_healthy = 0.2, pStart_healthy = 0.1, pComplete_healthy = 0.1, 
                          pSeek_exposure = 0.78, pStart_exposure = 0.7, pComplete_exposure = 0.6, pDeath = 0.16, 
                          pPrevent = 0.99, full_cost = 45, partial_cost = 25, mdv_campaign_budget = NULL , 
                          base_vax_cov = 0.05, vaccinate_dog_cost = c(1,3), target_vax_cov = 0.4 , pInvestigate = 0.5, 
                          pFound = 0.4, pTestable = 0.2, pFP = 0.05, rabies_inc= c(0.0075,0.0125)){
  
  # Step 1: Initialize horizon settings
  only_a_single_year <- initialize(horizon)

  # Step 2: Estimate dog population
  dog_pop <- estimate_dog_population(N, pop, HDR, horizon)
  
  # Step 3: Calculate vaccination coverage
  vax_cov <- calc_vax_coverage(base_vax_cov = base_vax_cov, target_vax_cov = target_vax_cov, 
                               mdv_campaign_budget = mdv_campaign_budget, vaccinate_dog_cost= vaccinate_dog_cost, 
                               dog_pop=dog_pop, horizon= horizon, discount=discount)
  
   #vax_cov <- c(0.15, 0.15, 0.2, 0.25, 0.6, 0.6, 0.3, 0, 0, 0.5, 0.5)
  # vax_cov <- c(0.1, 0.15, 0.25, 0.25, 0.5, 0.2, 0.1, 0.5, 0.4, 0.5, 0.5)

  
  # Step 4: Calculate vaccinated and susceptible dog populations
  vax_results <- calculate_vaccinated_and_susceptible(N, horizon, dog_pop, vax_cov)
  
  # Step 5: Calculate MDV campaign costs
  MDV_campaign_cost <- calculate_campaign_cost(N, horizon, mdv_campaign_budget, vax_results$ts_dogs_vaccinated, vaccinate_dog_cost)
  
  # Step 6A: Predict rabid dogs and human exposures
  rabies_results <- predict_rabies(N, horizon, vax_cov, dog_pop, rabies_inc, mu, k)
  
  
  # Step 6B:  Healthy bites
  ts_healthy_bites <- calculate_healthy_bites(N, horizon, dog_pop, pBite_healthy)
   

  # Step 7: Investigate rabid and healthy bites
  bite_investigation_results <- investigate_bites(N, horizon, rabies_results$ts_rabid_biting_dogs, 
                                                  pInvestigate, pFound, pTestable)
  
  
  
  # Step 8: Calculate healthcare-seeking behavior
  healthcare_results <- calculate_healthcare(N, horizon, rabies_results$ts_exposures, ts_healthy_bites, 
                                             pSeek_exposure, pStart_exposure, pComplete_exposure, 
                                             pSeek_healthy, pStart_healthy, pComplete_healthy)
  
  ts_start_PEP <- healthcare_results$ts_exp_start + healthcare_results$ts_healthy_start

  
  # Step 9: Predict deaths
    # some calcs first
  ts_exp_no_start = rabies_results$ts_exposures - healthcare_results$ts_exp_start
  ts_exp_incomplete = healthcare_results$ts_exp_start - healthcare_results$ts_exp_complete
  
    #  function
  deaths_PEP <- predict_deaths_and_deathsaverted_PEP(N, horizon, rabies_results$ts_exposures, ts_exp_no_start, 
                                                     healthcare_results$ts_exp_complete, ts_exp_incomplete, pDeath, pPrevent)
  
  # Step 10:  deaths averted by MDV
  ts_deaths_averted_MDV <- compute_deaths_averted_MDV(N, horizon, mdv_campaign_budget, target_vax_cov, base_vax_cov, 
                                                      dog_pop, rabies_inc, mu, k, pDeath, rabies_results$ts_exposures)
  
  # Step 11: deaths averted PEP (all exposures that start PEP*pDeath)
  exp_start <- healthcare_results$ts_exp_start 
  
  ts_deaths_averted_PEP <- matrix(
    rbinom(
      n     = length(exp_start),      # number of draws
      size  = as.vector(exp_start),   # number of trials
      prob  = pDeath                  # probability
    ),
    nrow = N,
    byrow = FALSE
  )
  
  ts_deaths_averted <- ts_deaths_averted_MDV + ts_deaths_averted_PEP
  
  
  # Step 12: Collect all variables starting with "ts_"
  
  #my_list<- ls(pattern="ts_")
  
  all_results <- c(vax_results, rabies_results, bite_investigation_results, healthcare_results, 
                   deaths_PEP, 
                   list(
                     ts_deaths_averted_MDV = ts_deaths_averted_MDV, 
                     ts_deaths_averted_PEP = ts_deaths_averted_PEP, 
                     ts_deaths_averted = ts_deaths_averted,
                     ts_start_PEP = ts_start_PEP,
                     ts_MDV_campaign_cost = MDV_campaign_cost,
                     ts_healthy_bites = ts_healthy_bites))
  
  
  # If only a single year was requested, extract the first column of each matrix
  if (only_a_single_year == 'yes') {
    all_results <- lapply(all_results, function(mat) mat[, 1, drop = FALSE])
  }
  
  
  # output
  return(all_results)
  
}


# #pep ########

# 
# 
# plot_ribbon <- function(mydata, x_axis, y_axis, xlab, ylab, palette = NULL) {
#   
#   #  color palette if none is provided
#   if (is.null(palette)) {
#     palette <- c("#8195b2", "#648c67", "orange2", "turquoise1") 
#   }
#   
#   ggplot(mydata, aes(x = {{ x_axis }}, y = {{ y_axis }}, group = scenario, color = scenario, fill = scenario)) +  
#     geom_line(size = 1) +  
#     geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.6, color = NA) +
#     labs(x = xlab, y = ylab) +
#     theme_bw() +
#     scale_y_continuous(labels = scales::comma) +
#     scale_color_manual(values = palette) +  # Apply custom colors
#     scale_fill_manual(values = palette) +   # Apply custom fill colors
#     theme(axis.text.x = element_text(angle = 0, hjust = 1))
# }
# 
# 
# 
# 
# # Ph #########
# 
status_quo_ph <- decision_tree(N = 10, pop = 500000, HDR = c(3,6), horizon = 5, mu = 0.38, k = 0.14,
                            pBite_healthy = 0.01, pSeek_healthy = 0.75,pStart_healthy = 1, pComplete_healthy = 0.9,
                            pSeek_exposure = 0.75, pStart_exposure = 1, pComplete_exposure = 0.9, pDeath = 0.17,
                            pPrevent = 0.986, full_cost = 45, partial_cost = 25, mdv_campaign_budget = NULL ,
                            base_vax_cov = 0.2, vaccinate_dog_cost = c(2,4), target_vax_cov = 0.2, pInvestigate = 0.5,
                            pFound = 0.4, pTestable = 0.2, pFP = 0.05)
# 
# 
# oh_ph <- decision_tree(N = 1000, pop = 500000, HDR = c(3,6), horizon = 5, mu = 0.38, k = 0.14, 
#                      pBite_healthy = 0.01, pSeek_healthy = 0.75,pStart_healthy = 1, pComplete_healthy = 0.9, 
#                      pSeek_exposure = 0.9, pStart_exposure = 1, pComplete_exposure = 0.9, pDeath = 0.17, 
#                      pPrevent = 0.986, full_cost = 45, partial_cost = 25, mdv_campaign_budget = NULL , 
#                      base_vax_cov = 0.2, vaccinate_dog_cost = c(2,4), target_vax_cov = 0.7, pInvestigate = 0.5, 
#                      pFound = 0.4, pTestable = 0.2, pFP = 0.05)
# 
# 
# mydata_sq_ph <-summarise_stochasticity(status_quo_ph$ts_exposures, "Status quo")
# mydata_oh_ph <- summarise_stochasticity(oh_ph$ts_exposures, "One Health" )
# 
# mydata_cbd_ph <- rbind(mydata_sq_ph, mydata_oh_ph)
# 
# plot_ribbon(mydata = mydata_cbd_ph, x_axis = year, y_axis = Median, xlab= "Year", ylab = "Exposures")
# 
# 
# # Plot with geom_line and geom_ribbon
# ggplot(mydata_cbd_ph, aes(x = year, y = Median, group = scenario, color = scenario, fill = scenario)) +  
#   geom_line(size = 1) +  # Line for Median values
#   geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.4, color = NA) +  # Confidence Interval ribbon
#   labs(
#     x = "Year",
#     y = "Exposures",
#   ) +
#   theme_bw() +
#   scale_y_continuous(labels = scales::comma) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# 
# # Ulanga ######
# # running as philippines temporarily
# 
# ulanga_sq <- decision_tree(N = 1000, pop = 250000, HDR = c(6, 15), horizon = 5, mu = 0.38, k = 0.14,
#                        pBite_healthy = 0.01, pSeek_healthy = 0.6,pStart_healthy = 0.2, pComplete_healthy = 0.2,
#                        pSeek_exposure = 0.7, pStart_exposure = 0.666667, pComplete_exposure = 0.3968, pDeath = 0.17,
#                        pPrevent = 0.986, full_cost = 45, partial_cost = 25, mdv_campaign_budget = NULL ,
#                        base_vax_cov = 0.05, vaccinate_dog_cost = c(2,4), target_vax_cov = 0, pInvestigate = 0.5,
#                        pFound = 0.4, pTestable = 0.2, pFP = 0.05)
# 
# ulanga_mdv <- decision_tree(N = 1000, pop = 250000, HDR = c(6, 15), horizon = 5, mu = 0.38, k = 0.14,
#                            pBite_healthy = 0.01, pSeek_healthy = 0.6,pStart_healthy = 0.2, pComplete_healthy = 0.2,
#                            pSeek_exposure = 0.7, pStart_exposure = 0.666667, pComplete_exposure = 0.3968, pDeath = 0.17,
#                            pPrevent = 0.986, full_cost = 45, partial_cost = 25, mdv_campaign_budget = NULL ,
#                            base_vax_cov = 0.05, vaccinate_dog_cost = c(2,4), target_vax_cov = 0.7, pInvestigate = 0.5,
#                            pFound = 0.4, pTestable = 0.2, pFP = 0.05)
# 
# 
# ulanga_free_pep <- decision_tree(N = 1000, pop = 250000, HDR = c(6, 15), horizon = 5, mu = 0.38, k = 0.14,
#                             pBite_healthy = 0.01, pSeek_healthy = 0.78, pStart_healthy = 0.2, pComplete_healthy = 0.2, 
#                             pSeek_exposure = 0.75, pStart_exposure = 0.899, pComplete_exposure = 0.542, pDeath = 0.17, 
#                             pPrevent = 0.986, full_cost = 15, partial_cost = 8.3, mdv_campaign_budget = NULL , 
#                             base_vax_cov = 0.05, vaccinate_dog_cost = c(2,4), target_vax_cov = 0, pInvestigate = 0.5, 
#                             pFound = 0.4, pTestable = 0.2, pFP = 0.05)
# 
# 
# ulanga_mdv_pep <- decision_tree(N = 1000, pop = 250000, HDR = c(6, 15), horizon = 5, mu = 0.38, k = 0.14,
#                                 pBite_healthy = 0.01, pSeek_healthy = 0.78, pStart_healthy = 0.2, pComplete_healthy = 0.2, 
#                                 pSeek_exposure = 0.75, pStart_exposure = 0.899, pComplete_exposure = 0.542, pDeath = 0.17, 
#                                 pPrevent = 0.986, full_cost = 15, partial_cost = 8.3, mdv_campaign_budget = NULL , 
#                                 base_vax_cov = 0.05, vaccinate_dog_cost = c(2,4), target_vax_cov = 0.7, pInvestigate = 0.5, 
#                                 pFound = 0.4, pTestable = 0.2, pFP = 0.05)
# 
# 
# # Define specific colors for each category
# color_mapping <- c(
#   "Status quo" = "grey63",   # 
#   "free PEP"   = "hotpink",   # 
#   "MDV"        = "#bc80bd",   # 
#   "One Health" = "#ffed6f"    # 
# )
# 
# 
# ulanga_sq_df <- summarise_stochasticity(ulanga_sq$ts_deaths, "Status quo")
# ulanga_fp_df <- summarise_stochasticity(ulanga_free_pep$ts_deaths, "free PEP" )
# ulanga_mdv_df <- summarise_stochasticity(ulanga_mdv$ts_deaths, "MDV")
# ulanga_oh <- summarise_stochasticity(ulanga_mdv_pep$ts_deaths, "One Health")
# 
# ulanga_cbd1 <- rbind(ulanga_sq_df, ulanga_fp_df)
# ulanga_cbd2 <- rbind(ulanga_sq_df, ulanga_mdv_df)
# ulanga_cbd3 <- rbind(ulanga_oh, ulanga_mdv_df)
# 
# 
# plot_ribbon(mydata = ulanga_sq_df, 
#             x_axis = year, 
#             y_axis = Median, 
#             xlab= "Year", 
#             ylab = "Deaths")+
#   scale_color_manual(values = color_mapping) +
#   scale_fill_manual(values = color_mapping) 
# 
# 
# a<- plot_ribbon(mydata = ulanga_cbd1, 
#             x_axis = year, 
#             y_axis = Median, 
#             xlab= "Year", 
#             ylab = "Deaths")+
#   scale_color_manual(values = color_mapping) +
#   scale_fill_manual(values = color_mapping) 
# 
# b<- plot_ribbon(mydata = ulanga_cbd2, 
#             x_axis = year, 
#             y_axis = Median, 
#             xlab= "Year", 
#             ylab = "Deaths")+
#   scale_color_manual(values = color_mapping) +
#   scale_fill_manual(values = color_mapping) 
# 
# c<- plot_ribbon(mydata = ulanga_cbd3, 
#             x_axis = year, 
#             y_axis = Median, 
#             xlab= "Year", 
#             ylab = "Deaths")+
#   scale_color_manual(values = color_mapping) +
#   scale_fill_manual(values = color_mapping) 
# 
# a/b/c + 
#   plot_annotation(tag_levels = 'A') 
# 
# # judicious use of resources
# ulanga_fp_df2 <- summarise_stochasticity(ulanga_sq$ts_exposures, "free PEP" )
# 
# 
# # Kitui ########
# 
# kitui_sq <- decision_tree(N = 1000, pop = 1250000, HDR = c(4,10), horizon = 5, mu = 0.38, k = 0.14,
#                            pBite_healthy = 0.01, pSeek_healthy = 0.6,pStart_healthy = 0.2, pComplete_healthy = 0.2,
#                            pSeek_exposure = 0.7, pStart_exposure = 0.666667, pComplete_exposure = 0.3968, pDeath = 0.17,
#                            pPrevent = 0.986, full_cost = 45, partial_cost = 25, mdv_campaign_budget = NULL ,
#                            base_vax_cov = 0.05, vaccinate_dog_cost = c(2,4), target_vax_cov = 0, pInvestigate = 0.5,
#                            pFound = 0.4, pTestable = 0.2, pFP = 0.05)
# 
# kitui_sq1 <- summarise_stochasticity(kitui_sq$ts_rabid_dogs, "Rabid dogs" )
# kitui_sq2 <- summarise_stochasticity(kitui_sq$ts_rabid_biting_dogs, "Rabid biting dogs" )
# 
# kitui_sq <- rbind(kitui_sq1, kitui_sq2)
# 
# plot_ribbon(kitui_sq, x_axis=year, y_axis=Median, xlab="Year", ylab="Value", palette = NULL) 
# 
# 
# # Pemba #####
# 
# pemba <- decision_tree(N = 100, pop = 540000, HDR = c(90, 110), horizon = 11, mu = 0.38, k = 0.14,
#                        pBite_healthy = 0.01, pSeek_healthy = 0.6,pStart_healthy = 0.2, pComplete_healthy = 0.2,
#                        pSeek_exposure = 0.7, pStart_exposure = 0.666667, pComplete_exposure = 0.3968, pDeath = 0.17,
#                        pPrevent = 0.986, full_cost = 45, partial_cost = 25, mdv_campaign_budget = NULL ,
#                        base_vax_cov = 0.05, vaccinate_dog_cost = c(2,4), target_vax_cov = 0.8, pInvestigate = 0.5,
#                        pFound = 0.4, pTestable = 0.2, pFP = 0.05)
# 
# 
# pemba_df<-summarise_stochasticity(pemba$ts_rabid_dogs, "Pemba")
# 
# 
# ggplot(pemba_df, aes(x = as.factor(year), y = Median, group = scenario, color = scenario, fill = scenario)) +
#   geom_line(size = 1) +  # Line for Median values
#   geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.2, color = NA) +  # Confidence Interval ribbon
#   labs(
#     x = "Year",
#     y = "Rabid dogs",
#   ) +
#   theme_bw() +
#   scale_y_continuous(labels = scales::comma) +
#   theme(axis.text.x = element_text(angle = 0, hjust = 1))
# 

# pemba_df$vax_cov <- c(0.15, 0.15, 0.2, 0.25, 0.6, 0.6, 0.3, 0, 0, 0.5, 0.5)
# max_dogs    <- max(pemba_df$UL, na.rm = TRUE)
# scaleFactor <- max_dogs
# 
# ggplot() +
#   # ribbon
#   geom_ribbon(
#     data = pemba_df,
#     aes(x = factor(year), ymin = LL, ymax = UL, group = scenario),
#     fill = "grey80", alpha = 0.3
#   ) +
#   # rabid dogs
#   geom_line(
#     data = pemba_df,
#     aes(x = factor(year), y = Median, color = "Rabid dogs", group = scenario),
#     size = 1
#   ) +
#   # coverage (with group=1!)
#   geom_line(
#     data = pemba_df,
#     aes(x = factor(year), y = vax_cov * scaleFactor,
#         color = "Vax coverage", group = 1),
#     size = 1, linetype = "dashed"
#   ) +
#   scale_y_continuous(
#     name    = "Rabid dogs",
#     labels  = scales::comma,
#     sec.axis = sec_axis(~ . / scaleFactor,
#                         name   = "Vaccination coverage",
#                         labels = scales::percent_format(accuracy = 1))
#   ) +
#   scale_color_manual(
#     name   = NULL,
#     values = c(
#       "Rabid dogs"   = "steelblue",
#       "Vax coverage" = "darkorange"
#     )
#   ) +
#   labs(x = "Year") +
#   theme_bw() +
#   theme(
#     axis.text.x     = element_text(hjust = 0.5),
#     legend.position = "top"
#   )





