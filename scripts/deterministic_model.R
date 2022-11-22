#' ---
#' title: "Deterministic Model Function"
#' author: "Martha Luka, adopted from Catherine S."
#' date: '`r format(Sys.Date(), '%Y-%m-%d')`'
#' output: html_document
#' ---
#' ### Function to generate deterministic values 

deterministic_decision_tree <- function(pop, HDR, 
                             vax_cov, incidence, 
                             P_bite_rabid, 
                             P_bite_healthy,
                             P_get_PEP_rabid_bite, 
                             P_get_PEP_healthy_bite,
                             P_death, P_prevent,
                             method_administered)
{
  
  # Model 1 Decision tree function uses the arguments:
    # pop - human population 
    # HDR_min - human:dog ratio 
    # vax_cov - dog vaccination coverage 
    # incidence - annual rabies incidence in dog population
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
  
  # use mean HDR to find dog pop
  dog_pop <- round(pop/HDR) # Calculate dog population 
  
  # # Estimate vaccinated and susceptible dogs in population 
  vax_dogs <- round(dog_pop * vax_coverage,0) 
  sus_dogs <- round(dog_pop - vax_dogs,0)
  
  # Calculate number of rabid dogs based on incidence
  rabid_dogs <- sus_dogs * incidence 
  
  # Project rabid_bites/exposures from rabid dogs and exposure incidence
  rabid_bites <- round(rabid_dogs * P_bite_rabid, 0) # People bitten by rabid dogs
  rabid_bites_inc <- round(rabid_bites * 100000 / pop,0) # Rabid bite incidence per 100,000 people 
  
  # Project non-exposures ie from bites from healthy dogs
  healthy_bites <- round((dog_pop-rabid_dogs) * P_bite_healthy, 0) # People bitten by healthy (non-infected/vaccinated) dogs
  healthy_bites_inc <- round(healthy_bites * 100000 / pop,0) # healthy_bites bite incidence per 100,000 people
  
  # Project rabid_bites who did and did NOT receive PEP 
  rabid_bites_no_PEP <- round(rabid_bites * (1 - P_get_PEP_rabid_bite),0)  # rabid_bites who did NOT receive PEP 
  rabid_bites_PEP <- round(rabid_bites * P_get_PEP_rabid_bite, 0)     # rabid_bites who received PEP
  #rabid_bites_PEP <- rabid_bites - rabid_bites_no_PEP    # rabid_bites who received PEP 
  
  # Project healthy_bites/non-exposures who did and did NOT receive PEP 
  healthy_bite_no_PEP <- round(healthy_bites * (1 - P_get_PEP_healthy_bite),0) # healthy_bites/Non-exposures who did NOT receive PEP
  healthy_bite_PEP <- round(healthy_bites * P_get_PEP_healthy_bite,0) # healthy_bites/Non-exposures who received PEP
  
  # Total people get PEP (healthy+rabid_bites)
  people_get_PEP <- healthy_bite_PEP + rabid_bites_PEP # Total people who get PEP (healthy_bites + rabid_bites)
  
  
  # Estimate deaths - and attribute causes
  deaths_no_PEP <- round(rabid_bites_no_PEP * P_death, 0) # Deaths because no PEP 
  PEP_fail <- rabid_bites_PEP * (1 - P_prevent) # Probability that PEP fails 
  deaths_PEP <- round(PEP_fail * P_death,0) # Deaths because PEP failed 
  all_deaths <- deaths_no_PEP + deaths_PEP # Total human rabies deaths 
  lives_saved <- deaths_no_PEP - deaths_PEP # Lives saved by PEP
  
  #Estimate economic costs incurred
  # If intramuscular injection is used, cost is 1 vial per patient
  # If intradermal injection is used, cost is 0.1 - 1 vials per patient. 
  # 0.1 ml is recommended for intradermal admin, 1 ml is the vial volume (doi.org/10.1016/j.vaccine.2018.08.034)
  # However, this has to be discarded 8 hours after reconstitution. Assuming no one else turns up for PEP, it'll be discarded
  
  if (method_administered == "intradermal"){
    vials_per_patient <- 0.2 #vials_per_patient per dose # assume 0.2 vials per patient for simplicity
    total_PEP <- ceiling(vials_per_patient * people_get_PEP)
  } else {                      # else assume intramuscular (the default in many countries)
    total_PEP <- ceiling(1 * people_get_PEP)
  }
  
  
  # Costs per death averted/ lives saved
  cost_per_life_saved <- ceiling(total_PEP / lives_saved)
  
  
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
               total_PEP_administered = total_PEP,
               cost_per_death_averted = cost_per_life_saved)
  )
}


#To do:
# incorporate incomplete and complete PEP regimens


