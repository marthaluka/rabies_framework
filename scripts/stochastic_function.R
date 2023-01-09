#' ---
#' title: "Stochastic Model Function"
#' author: "Martha Luka, adopted from Catherine S."
#' date: '`r format(Sys.Date(), '%Y-%m-%d')`'
#' output: html_document
#' ---
#' ### Function to generate stochastic values 

stochastic_decision_tree <- function(N, pop, 
                             HDR_min, HDR_max, 
                             vax_cov,
                             inc_min, inc_max, 
                             P_bite_rabid, 
                             # P_bite_healthy, # Now using `rgamma(1, shape=6.675, rate=2889.090)*dog_pop` to get healthy_bites
                             P_seek_PEP_rabid_bite,
                             P_initiate_PEP_rabid_bite, 
                             P_complete_PEP_rabid_bite,
                             P_seek_PEP_healthy_bite,
                             P_initiate_PEP_healthy_bite, 
                             P_complete_PEP_healthy_bite,
                             P_death, 
                             P_prevent_min, P_prevent_max)

{
  
  ###### 
  # stochastic_model function uses the arguments:
  # pop - human population 
  # HDR_min - minimum human:dog ratio
  # HDR_max - maximum human:dog ratio
  # vax_cov - average dog vaccination coverage 
  # inc_min - minimum annual rabies incidence in dog population
  # inc_max - maximum annual rabies incidence in dog population
  # P_bite_rabid - probability a rabid dog will bite 
  # P_bite_healthy - probability a healthy dog will bite
  # P_seek_PEP_rabid_bite - probability that a rabies exposure will receive PEP 
  # P_initiate_PEP_rabid_bite - probability that a rabies exposure will receive PEP 
  # P_complete_PEP_rabid_bite - probability that a rabies exposure will receive PEP 
  # P_seek_PEP_healthy_bite - probability that a healthy bite will receive PEP 
  # P_initiate_PEP_healthy_bite - probability that a healthy bite will receive PEP
  # P_complete_PEP_healthy_bite- probability that a healthy bite will receive PEP
  # P_death - probability of infection/death if bitten (in the absence of PEP)
  # P_prevent_min - minimum probability that PEP will prevent rabies infection. This is the lower value for incomplete PEP
  # P_prevent_max - maximum probability that PEP will prevent rabies infection. This is the upper value for complete PEP (1.0)

  # Model 1 outputs are annual estimates for: 
  # dog population 
  # rabid dogs over 1 year (incidence)
  # rabid_bites (total exposures over 1 year)
  # rabid_bites_per_capita (per 100,000)
  # rabid_bites that do (not) seek PEP 
  # rabid_bites that do (not) initiate PEP 
  # rabid_bites that do (not) complete PEP 
  # healthy_bites_per_capita (per 100,000)
  # healthy_bites that do (not) seek PEP 
  # healthy_bites that do (not) initiate PEP 
  # healthy_bites that do (not) complete PEP
  # total people that receive PEP (a sum of people who initiate PEp, for both healthy and rabid bites)
  # estimated human deaths 
  # lives saved (by PEP)
  # total PEP vials administered (for both intradermal and intramuscular)
  # cost per life saved (for both intradermal and intramuscular)
  ###### 
  
  

  # Explore uncertainty in HDR - uniform distribution w/ upper & lower limits 
  HDR <- runif(n=N, min = HDR_min, max = HDR_max)  
  dog_pop <- round(pop/HDR) # Calculate dog population 
  
  # Explore uncertainty in dog vaccination coverage - uniform distribution w/ upper & lower limits

  if (vax_cov > 0 & vax_cov < 1){
    vax_cov_max = vax_cov + 0.05
    vax_cov_min = vax_cov - 0.05
    }else{
    vax_cov_max = vax_cov
    vax_cov_min = vax_cov
    }
  
  vax_coverage <- runif(n = N, min = vax_cov_min, max = vax_cov_max) 
  vax_dogs <- dog_pop * vax_coverage 
  sus_dogs <- dog_pop - vax_dogs # Estimate susceptible dogs in population 
  
  # Generate variation in rabies incidence - uniform distribution w/ upper & lower limits 
  incidence <- runif(n=N, min = inc_min, max = inc_max) # Calculate incidence of rabies in dog population
  rabid_dogs <- round(sus_dogs * incidence,0) # Calculate number of rabid dogs based on incidence
  
  # Project rabid_bites/exposures from rabid dogs and exposure incidence
  rabid_bites <- rbinom(n = N, size = rabid_dogs, prob = P_bite_rabid) # People bitten by rabid dogs
  rabid_bites_inc <- round(rabid_bites * 100000 / pop,0) # Rabid bite incidence per 100,000 people 
  
  # Project non-exposures ie from bites from healthy dogs
  healthy_bites <- round(rgamma(N, shape=6.675, rate=2889.090)* dog_pop) 
  #healthy_bites <- rbinom(n = N, size = (dog_pop-rabid_dogs), prob = P_bite_healthy) # People bitten by healthy (non-infected/vaccinated) dogs
  healthy_bites_inc <- round(healthy_bites * 100000 / pop,0) # healthy_bites bite incidence per 100,000 people

  # Project rabid_bites who did (and did NOT) seek, initiate and complete PEP  
    # seek 
  rabid_bites_seek_PEP <- rbinom(n = N,  size = rabid_bites, prob = P_seek_PEP_rabid_bite)
  rabid_bites_do_not_seek_PEP <- rabid_bites - rabid_bites_seek_PEP 
    # initiate
  rabid_bites_initiate_PEP <- rbinom(n = N,  size = rabid_bites, prob = P_initiate_PEP_rabid_bite) # rabid_bites who received complete or incomplete PEP
  rabid_bites_do_not_initiate_PEP <- rabid_bites - rabid_bites_initiate_PEP # rabid_bites_do_not_seek_PEP + seek but do not initiate
    # complete  
  rabid_bites_complete_PEP <- rbinom(n = N,  size = rabid_bites_seek_PEP, prob = P_complete_PEP_rabid_bite)
  rabid_bites_incomplete_PEP <- rabid_bites_initiate_PEP - rabid_bites_complete_PEP
  
  # Project healthy_bites who did (and did NOT) seek, initiate and complete PEP  
    # seek 
  healthy_bites_seek_PEP <- rbinom(n = N,  size = round(healthy_bites), prob =  P_seek_PEP_healthy_bite) # R expects the second argument of rbinom, size, to be an integer
  healthy_bites_do_not_seek_PEP <- healthy_bites - healthy_bites_seek_PEP 
    # initiate
  healthy_bites_initiate_PEP <- rbinom(n = N,  size = healthy_bites, prob =  P_initiate_PEP_healthy_bite) # healthy_bites who received complete or incomplete PEP
  healthy_bites_do_not_initiate_PEP <- healthy_bites - healthy_bites_initiate_PEP # healthy_bites_do_not_seek_PEP + seek but do not initiate
    # complete  
  healthy_bites_complete_PEP <- rbinom(n = N,  size = healthy_bites, prob =  P_complete_PEP_healthy_bite)
  healthy_bites_incomplete_PEP <- healthy_bites_initiate_PEP - healthy_bites_complete_PEP
  
  # Total people get PEP (healthy+rabid_bites)
  people_get_PEP <- healthy_bites_initiate_PEP + rabid_bites_initiate_PEP # Total people who get either complete or incomplete PEP 
  
  # PEP doses administered to rabid bites
    # For incomplete PEP, one can have either have one or two doses.
    # More people have a single dose 2 doses. Therefore, sample with a probability 
          # First dose 0.899 (free) versus 0.873 (paid) --double check if this is correct 

  ### I'm not sure this is conceptually correct
  no_of_doses_for_incomplete_PEP_rabid_bite <- sample(c(1,2), size = N, replace = T, prob=c(0.9, 0.7)) # double check prob values
  one_dose_rabid_bite <- sum(no_of_doses_for_incomplete_PEP_rabid_bite==1)
  two_dose_rabid_bite <- sum(no_of_doses_for_incomplete_PEP_rabid_bite==2)
  
  PEP_doses_on_rabid_bites <- (rabid_bites_complete_PEP*3) + # assuming a three dose regimen
    (rabid_bites_incomplete_PEP * (one_dose_rabid_bite/N)*1) + # those who take only one dose
    (rabid_bites_incomplete_PEP * (two_dose_rabid_bite/N)*2) # two doses
  

  
  # PEP doses administered to healthy bites
  # For incomplete PEP, one can have either have one or two doses.
  # More people have a single dose 2 doses. Therefore, sample with a probability 

  ### I'm not sure this is conceptually correct
  no_of_doses_for_incomplete_PEP_healthy_bite <- sample(c(1,2), size = N, replace = T, prob=c(0.9, 0.7)) # these values are hypothetical
  one_dose_healthy_bite <- sum(no_of_doses_for_incomplete_PEP_healthy_bite==1)
  two_dose_healthy_bite <- sum(no_of_doses_for_incomplete_PEP_healthy_bite==2)
  
  PEP_doses_on_healthy_bites <- (healthy_bites_complete_PEP*3) + # assuming a three dose regimen
    (healthy_bites_incomplete_PEP * (one_dose_healthy_bite/N)*1) + # those who take only one dose
    (healthy_bites_incomplete_PEP * (two_dose_healthy_bite/N)*2) # two doses
 
  
  # Total PEP doses (on both rabid and healthy bites)
  Total_PEP_doses <- PEP_doses_on_healthy_bites + PEP_doses_on_rabid_bites
  
  # rabid_bites_no_PEP NOW rabid_bites_do_not_initiate_PEP
  # rabid_bites_PEP NOW rabid_bites_initiate_PEP
              # rabid_bites_no_PEP <- rbinom(n = N, size = rabid_bites, prob = (1 - P_get_PEP_rabid_bite)) # rabid_bites who did NOT receive PEP 
              # rabid_bites_PEP <- rbinom(n = N, size = rabid_bites, prob = P_get_PEP_rabid_bite) # rabid_bites who received PEP
              

  # Estimate deaths - and attribute causes
  deaths_no_PEP <- rbinom(n = N, size = rabid_bites_do_not_initiate_PEP, prob = P_death) # Deaths because no PEP 
  
  P_prevent <- runif(n = N, min = P_prevent_min, max = P_prevent_min) 
  PEP_fail <- rbinom(n = N, size = rabid_bites_initiate_PEP, prob = (1 - P_prevent)) # Probability that PEP fails 
  
  deaths_PEP <- rbinom(n = N, size = PEP_fail, prob = P_death) # Deaths because PEP failed 
  all_deaths <- deaths_no_PEP + deaths_PEP # Total human rabies deaths 
  
  lives_saved <- rabid_bites_initiate_PEP * P_prevent

  
  #Estimate economic costs incurred
      # If intramuscular injection is used, cost is 1 vial per dose 
      # If intradermal injection is used, cost is 0.1 - 1 vials per dose 
      # 0.1 ml is recommended for intradermal admin, 1 ml is the vial volume (doi.org/10.1016/j.vaccine.2018.08.034)
      # However, this has to be discarded 8 hours after reconstitution. Assuming no one else turns up for PEP, it'll be discarded
      
  #vials_per_dose <- runif(n=N, min = 0.1, max = 1)  #vials per dose

  vials_per_dose <- 1/rbinom(n = N, size = 10, prob = 0.5)
  total_PEP_intradermal <- vials_per_dose * Total_PEP_doses
  total_PEP_intramuscular <- Total_PEP_doses
  
  # Costs per death averted/ lives saved
  
  cost_per_life_saved_intradermal <- ceiling(total_PEP_intradermal / lives_saved) * 15 # 15 USD per vial
  cost_per_life_saved_intramuscular <- ceiling(total_PEP_intramuscular / lives_saved) * 15 # 15 USD per vial

# Output results
  return( 
    data.frame(dog_population = dog_pop,
               rabid_dogs = rabid_dogs,
               total_rabid_bites = rabid_bites,
               total_healthy_bites = healthy_bites,
               rabid_bites_per_capita = rabid_bites_inc,
               healthy_bites_per_capita = healthy_bites_inc,
               rabid_bites_no_PEP = rabid_bites_do_not_initiate_PEP,
               rabid_bites_seek_PEP = rabid_bites_seek_PEP,
               rabid_bites_initiate_PEP = rabid_bites_initiate_PEP,
               rabid_bites_complete_PEP = rabid_bites_complete_PEP,
               healthy_bites_no_PEP = healthy_bites_do_not_initiate_PEP,
               healthy_bites_seek_PEP = healthy_bites_seek_PEP,
               healthy_bites_initiate_PEP = healthy_bites_initiate_PEP,
               healthy_bites_complete_PEP = healthy_bites_complete_PEP,
               total_people_PEP = people_get_PEP,
               deaths_no_PEP = deaths_no_PEP,
               rabies_deaths = all_deaths,
               lives_saved = lives_saved,
               total_PEP_intradermal = total_PEP_intradermal,
               total_PEP_intramuscular = total_PEP_intramuscular,
               cost_per_life_saved_intradermal = cost_per_life_saved_intradermal,
               cost_per_life_saved_intramuscular = cost_per_life_saved_intramuscular)
  )
}
