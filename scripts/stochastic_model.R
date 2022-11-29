#' ---
#' title: "Stochastic Model Function"
#' author: "Martha Luka, adopted from Catherine S."
#' date: '`r format(Sys.Date(), '%Y-%m-%d')`'
#' output: html_document
#' ---
#' ### Function to generate stochastic values 

stochatic_decision_tree <- function(N, pop, 
                             HDR_min, HDR_max, 
                             vax_cov_min, vax_cov_max,
                             inc_min, inc_max, 
                             P_bite_rabid, 
                             P_bite_healthy,
                             P_get_PEP_rabid_bite, 
                             P_get_PEP_healthy_bite,
                             P_death, P_prevent)
{
  
  # Model 1 Decision tree function uses the arguments:
    # N - number of iterations  
    # pop - human population 
    # HDR_min (lower limit) and HDR_max (upper limit) - human:dog ratio 
    # vax_cov_min (lower limit) and vax_cov_max (upper limit) - dog vaccination coverage 
    # inc_min (lower limit) and inc_max (upper limit) - annual rabies incidence in dog population
    # P_bite_rabid - probability a rabid dog will bite 
    # P_bite_healthy - probability a healthy dog will bite
    # P_get_PEP_rabid_bite - probability that a rabies exposure will receive PEP 
    # P_get_PEP_healthy_bite - probability that a healthy bite will receive PEP
    # P_death - probability of infection/death if bitten (in the absence of PEP)
    # P_prevent - probability that PEP will prevent rabies infection 
    # method_administered - the method used to administer PEP (either 'intradermal' or 'intramuscular')
  
  # Model 1 outputs are annual estimates for: 
    # dog population 
    # rabid dogs (over 1 year)
    # rabid_bites (total exposures over 1 year)
    # rabid_bites_per_capita (per 100,000)
    # rabid_bites that do not seek PEP 
    # estimated human deaths 
    # annual exposure incidence (per 100,000 persons)
    # rabies incidence in dog populaton 
  

  # Explore uncertainty in HDR - uniform distribution w/ upper & lower limits 
  HDR <- runif(n=N, min = HDR_min, max = HDR_max)  
  dog_pop <- round(pop/HDR) # Calculate dog population 
  
  # Explore uncertainty in dog vaccination coverage - uniform distribution w/ upper & lower limits
  vax_coverage <- runif(n = N, min = vax_cov_min, max = vax_cov_max) 
  vax_dogs <- dog_pop * vax_coverage #** Estimate vaccinated dogs in population - is there a risk of non-integer dogs?**
  sus_dogs <- dog_pop - vax_dogs # Estimate susceptible dogs in population 
  
  # Generate variation in rabies incidence - uniform distribution w/ upper & lower limits 
  incidence <- runif(n=N, min = inc_min, max = inc_max) # Calculate incidence of rabies in dog population
  rabid_dogs <- round(sus_dogs * incidence,0) # Calculate number of rabid dogs based on incidence
  
  # Project rabid_bites/exposures from rabid dogs and exposure incidence
  rabid_bites <- rbinom(n = N, size = round((rabid_dogs),0), prob = P_bite_rabid) # People bitten by rabid dogs
  rabid_bites_inc <- round(rabid_bites * 100000 / pop,0) # Rabid bite incidence per 100,000 people 
  
  # Project non-exposures ie from bites from healthy dogs
  healthy_bites <- rbinom(n = N, size = round((dog_pop-rabid_dogs),0), prob = P_bite_healthy) # People bitten by healthy (non-infected/vaccinated) dogs
  healthy_bites_inc <- round(healthy_bites * 100000 / pop,0) # healthy_bites bite incidence per 100,000 people

  # Project rabid_bites who did and did NOT receive PEP 
  rabid_bites_no_PEP <- rbinom(n = N, size = rabid_bites, prob = (1 - P_get_PEP_rabid_bite)) # rabid_bites who did NOT receive PEP 
  rabid_bites_PEP <- rbinom(n = N, size = rabid_bites, prob = P_get_PEP_rabid_bite) # rabid_bites who received PEP
  #rabid_bites_PEP <- rabid_bites - rabid_bites_no_PEP # rabid_bites who received PEP 

  # Project healthy_bites/non-exposures who did and did NOT receive PEP 
  healthy_bite_no_PEP <- rbinom(n = N, size = healthy_bites, prob = (1 - P_get_PEP_healthy_bite)) # healthy_bites/Non-exposures who did NOT receive PEP
  healthy_bite_PEP <- rbinom(n = N, size = healthy_bites, prob = P_get_PEP_healthy_bite) # healthy_bites/Non-exposures who received PEP

  # Total people get PEP (healthy+rabid_bites)
  people_get_PEP <- healthy_bite_PEP + rabid_bites_PEP # Total people who get PEP (healthy_bites + rabid_bites)


  # Estimate deaths - and attribute causes
  deaths_no_PEP <- rbinom(n = N, size = rabid_bites_no_PEP, prob = P_death) # Deaths because no PEP 
  PEP_fail <- rbinom(n = N, size = rabid_bites_PEP, prob = 1 - P_prevent) # Probability that PEP fails # This is unclear to me - to follow up
  deaths_PEP <- rbinom(n = N, size = PEP_fail, prob = P_death) # Deaths because PEP failed 
  all_deaths <- deaths_no_PEP + deaths_PEP # Total human rabies deaths 
  lives_saved <- deaths_no_PEP - deaths_PEP #** Check this - it is conceptually incorrect (see comments in deterministic code) **

  #Estimate economic costs incurred
    # If intramuscular injection is used, cost is 1 vial per patient
    # If intradermal injection is used, cost is 0.1 - 1 vials per patient. 
    # 0.1 ml is recommended for intradermal admin, 1 ml is the vial volume (doi.org/10.1016/j.vaccine.2018.08.034)
    # However, this has to be discarded 8 hours after reconstitution. Assuming no one else turns up for PEP, it'll be discarded

  vials_per_patient <- runif(n=N, min = 0.1, max = 1)  #vials_per_patient per dose
  total_PEP_intradermal <- ceiling(vials_per_patient * people_get_PEP)
  total_PEP_intramuscular <- ceiling(1 * people_get_PEP)

# Costs per death averted/ lives saved
  cost_per_life_saved_intradermal <- ceiling(total_PEP_intradermal / lives_saved) * 15 # 15 USD per vial
  cost_per_life_saved_intramuscular <- ceiling(total_PEP_intramuscular / lives_saved) * 15 # 15 USD per vial

# Output results
  return( 
    data.frame(dog_population = dog_pop,
               rabies_incidence = incidence,
               rabid_dogs = rabid_dogs,
               total_rabid_bites = rabid_bites,
               total_healthy_bites = healthy_bites,
               rabid_bites_per_capita = rabid_bites_inc,
               healthy_bites_per_capita = healthy_bites_inc,
               rabid_bites_PEP = rabid_bites_PEP,
               rabid_bites_no_PEP = rabid_bites_no_PEP,
               healthy_bite_PEP = healthy_bite_PEP,
               healthy_bite_no_PEP = healthy_bite_no_PEP,
               total_people_PEP = people_get_PEP,
               deaths_no_PEP = deaths_no_PEP,
               PEP_fail = PEP_fail,
               deaths_PEP = deaths_PEP,
               rabies_deaths = all_deaths,
               lives_saved = lives_saved,
               total_PEP_intradermal = total_PEP_intradermal,
               total_PEP_intramuscular = total_PEP_intramuscular,
               cost_per_life_saved_intradermal = cost_per_life_saved_intradermal,
               cost_per_life_saved_intramuscular = cost_per_life_saved_intramuscular)
    )
}


#To do:
    # incorporate incomplete and complete PEP regimens



# check if works
stochatic_decision_tree(N=10, pop=150000, 
      HDR_min=10, HDR_max=40, 
      vax_cov_min=0, vax_cov_max=0.3,
      inc_min=0.05, inc_max=0.1, 
      P_bite_rabid=0.1, 
      P_bite_healthy=0.3,
      P_get_PEP_rabid_bite=0.6, 
      P_get_PEP_healthy_bite=0.1,
      P_death=0.9, P_prevent=0.98,
      method_administered="intradermal")

# Somewhere write out your parameters and run the model to report results!
