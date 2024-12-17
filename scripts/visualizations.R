

# Libraries ####

require(pacman)
pacman::p_load(rstantools, 
               brms,    #bayesian regression models
               CGPfunctions, # slope charts
               purrr,
               grid,  # multiple grobs
               gridGraphics,
               gridExtra, # multiple grobs
               patchwork, # multiple plots
               sf
               )

# Visualization 1 - Predicted cases given X vaccination coverage ####

# Forecasting rabies cases over X years, given Y% dog vaccination coverage

## Deterministic ####

vax_coverage_over_x_years <- function(target_coverage, no_of_years){
  # assume it takes 3 years to hit target vaccination coverage
  list1<- map2(0, target_coverage, seq, length.out = 3)
  list2<- rep(target_coverage, (no_of_years-3))
  annual_vax_cov<- unlist(append(list1, list2))
  return(annual_vax_cov)
  }

# Set up prediction model
predict_cases_deterministic <- function(target_coverage, no_of_years, dog_pop){
  # model inputs
  vax_model <- readRDS("./data/cases_from_vax_model.rds")
  vax_case_model <- readRDS("./data/cases_from_vax+cases_model.rds")
  # model output
  cases <- rep(NA, no_of_years)
  vc_last_year <- vax_coverage_over_x_years(target_coverage, no_of_years)
  new_data <- data.frame(vax_last_year=vc_last_year[1],dogs=dog_pop)
  cases[1] <- mean(posterior_epred(vax_model, newdata = new_data))
  for(year in 2:no_of_years){
    new_data <- data.frame(vax_last_year=vc_last_year[year],cases_last_year=cases[year-1],dogs=dog_pop)
    cases[year] <- colMeans(posterior_epred(vax_case_model, newdata = new_data))
    }
  return(cases)
  }


predicted_cases_deterministic <- predict_cases_deterministic(target_coverage=0.7, no_of_years=8, dog_pop=200000)

plot(predicted_cases_deterministic,type="l", ylab="Cases",xlab="Year")


## Stochastic #####

# Set up model
predict_cases_stochastic <- function(nreps=1000, target_coverage, no_of_years, dog_pop){
  set.seed(0)
  # model inputs
  vax_model_samples <- read.csv("./data/cases_from_vax_par_samples.csv")
  vax_case_model_samples <- read.csv("./data/cases_from_vax+cases_par_samples.csv")
  # model output
  cases <- rep(NA, no_of_years)
  vc_last_year <- vax_coverage_over_x_years(target_coverage, no_of_years)
  cases_mat <- matrix(NA,nrow=nreps,ncol=no_of_years)

  # Simulate from models once for a district
  pars_sim <- vax_model_samples[sample.int(nrow(vax_model_samples),size=1),]
  mu <- exp(sum(pars_sim[1:2]*c(1,vc_last_year[1]),log(dog_pop)))
  cases[1] <- rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[3]))
  pars_sim <- vax_case_model_samples[sample.int(nrow(vax_case_model_samples),size=1),]
  for(i in 2:no_of_years){
    mu <- exp(sum(pars_sim[1:3]*c(1,vc_last_year[i],log(cases[i-1]+1)),log(dog_pop)))
    cases[i] <- rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[4]))
  }

  # How does this look of we do it many times?
  for(rep in 1:nreps){
    pars_sim <- vax_model_samples[sample.int(nrow(vax_model_samples),size=1),]
    mu <- exp(sum(pars_sim[1:2]*c(1,vc_last_year[1]),log(dog_pop)))
    cases_mat[rep,1] <- rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[3]))
    pars_sim <- vax_case_model_samples[sample.int(nrow(vax_case_model_samples),size=1),]
    for(year in 2:no_of_years){
      mu <- exp(sum(pars_sim[1:3]*c(1,vc_last_year[year],log(cases[year-1]+1)),log(dog_pop)))
      cases_mat[rep,year] <- rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[4]))
    }
  }

  lower <- upper <- med <- rep(NA,no_of_years) # get median and 95 percentile limits
  quantiles <- matrix(NA,nrow=3,ncol=no_of_years)
  for(year in 1:no_of_years){
    quantiles[,year] <- quantile(cases_mat[,year],c(0.025,0.5,0.975))
  }
  return(quantiles)
}

# dog_pop
dog_pop_df <- output_stoch_model[1:nrow(east_africa_shp),] %>%
  st_drop_geometry()  %>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(dog_pop=sum(dog_population_mean),.groups = 'drop') %>%
  as.data.frame()

# plot
plot_prediction <- function(no_of_years=8, country){
  predicted_cases_stochastic <-predict_cases_stochastic(nreps=1000, target_coverage=0.7, no_of_years=8, dog_pop=dog_pop_df$dog_pop[dog_pop_df$Country==country])
  plot(predicted_cases_stochastic[2,],ylim=c(0,max(predicted_cases_stochastic)),type="l",bty="l",ylab="Cases",xlab="Year",
       main="Predicted cases with 70% dog vaccination")
  polygon(c(1:no_of_years,rev(1:no_of_years)),c(predicted_cases_stochastic[1,],rev(predicted_cases_stochastic[3,])),
          col=adjustcolor( "#E69F00", alpha.f = 0.4),border=adjustcolor( "#E69F00", alpha.f = 0.4))
  lines(predicted_cases_stochastic[2,],lwd=2,col="black")
}


plot_prediction(no_of_years=8, country = "Kenya")

# Visualization 2 - Slope chart comparing different policy choices ####

# Slope chart

   ## extract data
filter_policy_summary_df <- function(country, big_df){
  policy_summary_df <- output_stoch_model %>%
    dplyr::filter(Country == country,
                  vax_cov == 0) %>%
    st_drop_geometry() %>%
    dplyr::select(policy_choice, total_people_PEP_mean,
                  lives_saved_mean, rabies_deaths_mean) %>%
    #drop_na()  %>%
    dplyr::group_by(policy_choice) %>%
    dplyr::summarise(Total_people_PEP = sum(total_people_PEP_mean),
              Lives_saved = sum(lives_saved_mean),
              Rabies_deaths = sum(rabies_deaths_mean),
              .groups = 'drop') %>%
    gather(., outcome, no_of_people, Total_people_PEP:Rabies_deaths) %>%
    mutate_at(
      "policy_choice", recode,
      "Offered free of charge" = "Free of charge",
      "Offered under status quo" = "Status quo")

   ## use whole numbers
  policy_summary_df$no_of_people <- round(policy_summary_df$no_of_people, 0)

   ## add commas for aesthetics
  policy_summary_df$label <- scales::comma(policy_summary_df$no_of_people)
  
  return(policy_summary_df)
}



   ## plot

plot_b_function <- function(country, big_df){
  
  policy_summary_df <- filter_policy_summary_df(country, big_df)

  plot_b <- newggslopegraph(dataframe = policy_summary_df,
                Times = policy_choice,
                Measurement = no_of_people,
                Grouping = outcome,
                Data.label = label,
                Title = "Policy choices for PEP",
                SubTitle = NULL,
                Caption = NULL,
                LineThickness = 1.2,
                XTextSize = 10,    # Size of the times
                YTextSize = 3,     # Size of the groups
                TitleTextSize = 14,
                SubTitleTextSize = 12,
                CaptionTextSize = 10,
                TitleJustify = "center",
                SubTitleJustify = "right",
                CaptionJustify = "left",
                DataTextSize = 3.5,
                DataLabelLineSize = 0
                )
  return(plot_b)
}

plot_b_function(country="Kenya", big_df = output_stoch_model)

# Visualization 3 - Boxplot comparing costs of PEP administration choices ####
## extract data


filter_pep_admin_summary_df <- function(country, big_df){
  pep_admin_summary_df <- output_stoch_model %>%
    dplyr::filter(Country == country,
                vax_cov == 0,
                policy_choice == "Offered under status quo") %>%
    st_drop_geometry() %>%
    dplyr::select(total_PEP_intradermal_mean, total_PEP_intradermal_UL, total_PEP_intradermal_LL,
                total_PEP_intramuscular_mean, total_PEP_intramuscular_UL, total_PEP_intramuscular_LL) %>%
    drop_na() %>%
    summarise(across(everything(), ~ sum(.))) %>%
    mutate_all(.,function(col){15*col})   #USD value of PEP vials

  intraM <- data.frame("Intramuscular", pep_admin_summary_df$total_PEP_intramuscular_mean, pep_admin_summary_df$total_PEP_intramuscular_UL, pep_admin_summary_df$total_PEP_intramuscular_LL)
  names(intraM) <- c("PEP_admin_route", "Mean", "upper_limit", "lower_limit")
  intraD<- data.frame("Intradermal", pep_admin_summary_df$total_PEP_intradermal_mean, pep_admin_summary_df$total_PEP_intradermal_UL, pep_admin_summary_df$total_PEP_intradermal_LL)
  names(intraD) <- c("PEP_admin_route", "Mean", "upper_limit", "lower_limit")

  pep_admin_summary_df <- rbind(intraM, intraD)
  
  return(pep_admin_summary_df)
}

plot_c_function <- function(country, big_df){
  
  filter_pep_admin_summary_df(country, big_df) %>%
    ggplot(., aes(x=PEP_admin_route, y=Mean, color=PEP_admin_route)) +
    geom_point(size=3)+
    geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit), width=.2,
                  position=position_dodge(0.05)) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values=c('#E69F00', '#999999'))+
    labs(title = "Estimated national costs of PEP annually",
         y = "US dollars")+
    theme(
      axis.text.x = element_text(angle=0, color="black", size=13),
      axis.title.x = element_blank(),
      axis.title.y = element_text(color="black", size=14, face="bold"),
      axis.text.y = element_text(color="black", size=13),
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "none"
    )
}


plot_c_function(country="Kenya", big_df = output_stoch_model)


# create a grid of the three plots

Create_plot_grid <- function(no_of_years=8, big_df = output_stoch_model, country){
  a <- plot_prediction(no_of_years, country = country)
  b <- plot_b_function(country=country, big_df)
  c <- plot_c_function(country=country, big_df)
  
  plot_grid <- (b+c) / ~plot_prediction(no_of_years, country = country)
  
  return(plot_grid)
}


KE_plot <- Create_plot_grid(country="Kenya")
UG_plot <- Create_plot_grid(country="Uganda")
TZ_plot <- Create_plot_grid(country="Tanzania")


# remove objects we wont need downstream from memory
rm(list=setdiff(ls(), c("east_africa_shp",  "output_stoch_model", "output_det_model",
                        "vax_covs", "KE_plot", "UG_plot", "TZ_plot")))


