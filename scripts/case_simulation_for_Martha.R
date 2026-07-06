
set.seed(0)
library(brms)

# Load models and extract parameter samples
vax_model <- readRDS("./data/cases_from_vax_model.rds")
vax_case_model <- readRDS("./data/cases_from_vax+cases_model.rds")
vax_model_samples <- posterior_samples(vax_model)[,1:3]
vax_case_model_samples <- posterior_samples(vax_case_model)[,1:4]



# Deterministic
#___________________


# Set up model inputs/outputs for a district
years <- 8
cases <- rep(NA, years)
vc_last_year <- c(0,0,0.7,0.7,0.7,0,0,0)
dogs <- 60000

# Predict cases
incidence_adjust <- 0.0001394842
new_data <- data.frame(vax_last_year=vc_last_year[1],dogs=dogs) 
cases[1] <- mean(posterior_epred(vax_model, newdata = new_data))
for(year in 2:years){
  new_data <- data.frame(vax_last_year=vc_last_year[year],log_incidence_last_year=log(cases[year-1]/dogs+incidence_adjust),dogs=dogs) 
  cases[year] <- colMeans(posterior_epred(vax_case_model, newdata = new_data))
}
plot(cases,type="l")


# Stochastic
#___________________

# Set up model inputs for a district
years <- 8
cases <- rep(NA, years)
vc_last_year <- c(0,0,0.7,0.7,0.7,0,0,0)
dogs <- 60000

# Simulate from models once for a district
pars_sim <- vax_model_samples[sample.int(nrow(vax_model_samples),size=1),]
mu <- exp(sum(pars_sim[1:2]*c(1,vc_last_year[1]),log(dogs)))
cases[1] <- rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[3]))
pars_sim <- vax_case_model_samples[sample.int(nrow(vax_case_model_samples),size=1),]
for(i in 2:years){
  mu <- exp(sum(pars_sim[1:3]*c(1,vc_last_year[i],log(cases[i-1]/dogs+incidence_adjust)),log(dogs)))
  cases[i] <- rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[4]))
}
par(mfrow=c(1,2))
plot(cases,type="l")


# How does this look if we do it many times?
nreps <- 5000
cases_mat <- matrix(NA,nrow=nreps,ncol=years)
for(rep in 1:nreps){
  pars_sim <- vax_model_samples[sample.int(nrow(vax_model_samples),size=1),]
  mu <- exp(sum(pars_sim[1:2]*c(1,vc_last_year[1]),log(dogs)))
  cases_mat[rep,1] <- rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[3]))
  pars_sim <- vax_case_model_samples[sample.int(nrow(vax_case_model_samples),size=1),]
  for(year in 2:years){
    mu <- exp(sum(pars_sim[1:3]*c(1,vc_last_year[year],log(cases_mat[rep,year-1]/dogs+incidence_adjust)),log(dogs)))
    cases_mat[rep,year] <- rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[4]))
  }  
}

# get median and 95 percentile limits
quantiles <- matrix(NA,nrow=3,ncol=years)
for(year in 1:years){
  quantiles[,year] <- quantile(cases_mat[,year],c(0.025,0.5,0.975))
}
  
plot(quantiles[2,],ylim=c(0,max(quantiles)),type="l",bty="l",ylab="Cases",xlab="Year")
polygon(c(1:years,rev(1:year)),c(quantiles[1,],rev(quantiles[3,])),col="lightblue",border="lightblue")
lines(quantiles[2,],lwd=2,col="navy")



# Reactive vaccination
#___________________

# Martha's semi-deterministic version
#------------

# Define function
semideterministic <- function(vc_last_year = c(0,0,0.7,0.7,0.7,0,0,0),
                              dogs=60000,
                              max_vc=0.9,
                              dogs_per_reactive_vax=20,
                              max_reactive_vaccinations=360,
                              mu = 0.38, k = 0.72,
                              pSeek_exposure = 0.9,
                              pInvestigate = 0.5, pFound = 0.4, pTestable = 0.2,
                              max_incidence = 0.0125,
                              nreps = 5000){
  
  # definitions  
  years <- length(vc_last_year)
  ibcm_efficiency <- pInvestigate * pFound * pTestable
  cases <- rep(NA, years)
  
  # Get expected cases year by year, adjusting vaccination based on the previous year
  incidence_adjust <- 0.0001394842
  vc_last_year_reactive <- vc_last_year
  for(year in 1:years){
    if(year==1){
      new_data <- data.frame(vax_last_year=vc_last_year_reactive[1],dogs=dogs) 
      cases[1] <- min(mean(posterior_epred(vax_model, newdata = new_data)),max_incidence*dogs)
    }else{
      new_data <- data.frame(vax_last_year=vc_last_year_reactive[year],log_incidence_last_year=log(cases[year-1]/dogs+incidence_adjust),dogs=dogs) 
      cases[year] <- min(mean(posterior_epred(vax_case_model, newdata = new_data)),max_incidence*dogs)
    }
    reactive_vc <- (min(cases[year]* mu * pSeek_exposure * ibcm_efficiency, max_reactive_vaccinations) * dogs_per_reactive_vax)/dogs # This isn't perfect - it assumes a 1:1 relationship between humans investigated and dogs investigated
    vc_last_year_reactive[year+1] <- min(max_vc,vc_last_year_reactive[year+1]+reactive_vc) # adjust vaccination this year to affect next years cases - assumes no revaccination
  }
  
  # Now run through the stochastic model using the new vax estimates
  cases_mat <- matrix(NA,nrow=nreps,ncol=years)
  for(rep in 1:nreps){
    pars_sim <- vax_model_samples[sample.int(nrow(vax_model_samples),size=1),]
    mu <- exp(sum(pars_sim[1:2]*c(1,vc_last_year[1]),log(dogs)))
    cases_mat[rep,1] <- min(rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[3])),max_incidence*dogs)
    pars_sim <- vax_case_model_samples[sample.int(nrow(vax_case_model_samples),size=1),]
    for(year in 2:years){
      mu <- exp(sum(pars_sim[1:3]*c(1,vc_last_year_reactive[year],log(cases_mat[rep,year-1]/dogs+incidence_adjust)),log(dogs)))
      cases_mat[rep,year] <- min(rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[4])),max_incidence*dogs)
    }  
  }
  
  # get median and 95 percentile limits
  quantiles <- matrix(NA,nrow=3,ncol=years)
  for(year in 1:years){
    quantiles[,year] <- quantile(cases_mat[,year],c(0.025,0.5,0.975))
  }
  
  return(quantiles)
}


# Test
vc_last_year <- rep(0.2,8) # Try different underlying vaccination efforts
system.time({
  out_semidet <- semideterministic(vc_last_year = vc_last_year)
  out_semidet_no_reactive <- semideterministic(vc_last_year = vc_last_year,max_reactive_vaccinations = 0)
})

# Plot
par(mfrow=c(1,1))
plot(NA,ylim=c(0,max(out_semidet,out_semidet_no_reactive)),xlim=c(1,years),type="l",bty="l",ylab="Cases",xlab="Year")
polygon(c(1:years,rev(1:year)),c(out_semidet[1,],rev(out_semidet[3,])),col=scales::alpha("dodgerblue",0.3),border=NA, density = 10, angle = -45,lwd=4)
polygon(c(1:years,rev(1:year)),c(out_semidet_no_reactive[1,],rev(out_semidet_no_reactive[3,])),col=scales::alpha("orange",0.3),border=NA, density = 10, angle = 45,lwd=4)
lines(out_semidet_no_reactive[2,],lwd=2,col="darkorange",lty=2)
lines(out_semidet[2,],lwd=2,col="navy")



# Fully stochastic/dynamic version
#------------

source("./scripts/HelperFun.R")

# Define function
stochastic <- function(vc_last_year = c(0,0,0.7,0.7,0.7,0,0,0),
                       dogs=60000,
                       max_vc=0.9,
                       dogs_per_reactive_vax=20,
                       max_reactive_vaccinations=360,
                       mu_bites = 0.38, k = 0.72,
                       pSeek_exposure = 0.9,
                       pInvestigate = 0.5, pFound = 0.4, pTestable = 0.2,
                       max_incidence = 0.0125,
                       nreps = 5000){
  
  # definitions  
  years <- length(vc_last_year)
  cases <- rep(NA, years)
  incidence_adjust <- 0.0001394842
  
  # Matrix for storing results
  cases_mat <- matrix(NA,nrow=nreps,ncol=years)
  
  # Run model for each rep...
  for(rep in 1:nreps){
    vc_last_year_reactive <- vc_last_year # vector of vaccination coverage to be adjusted each year based on cases
    
    # ...and each year
    for(year in 1:years){
      
      # Get cases and bitten this year
      if(year==1){
        pars_sim <- vax_model_samples[sample.int(nrow(vax_model_samples),size=1),]
        mu <- exp(sum(pars_sim[1:2]*c(1,vc_last_year[1]),log(dogs)))
        cases_mat[rep,1] <- min(rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[3])),max_incidence*dogs)
        pars_sim <- vax_case_model_samples[sample.int(nrow(vax_case_model_samples),size=1),]
        
      }else{
        mu <- exp(sum(pars_sim[1:3]*c(1,vc_last_year_reactive[year],log(cases_mat[rep,year-1]/dogs+incidence_adjust)),log(dogs)))
        cases_mat[rep,year] <- min(rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[4])),max_incidence*dogs)
      }
      
      # Get people bitten
      exposures <- nBitesBiters(cases_mat[rep,year],mu_bites,k)
      n_exposures <- exposures$nBites
      
      # Get people seeking care
      n_exp_seek_care <- rbinom(1,n_exposures,pSeek_exposure)
      
      # Get biters of people seeking care
      b <- exposures$bites_by_dog
      biters_sought_care <- sample(rep.int(seq_along(b), b),n_exp_seek_care)
      
      # Get exposures that had an animal investigation
      exp_seek_care_inv <- rbinom(1,n_exp_seek_care,pInvestigate)
      
      # Get biters of exposures that had an animal investigation
      biters_investigated <- biters_sought_care[sample.int(length(biters_sought_care),exp_seek_care_inv)]
      n_biters_investigated <- length(unique(biters_investigated))
      
      # Get investigated biters that were found and testable
      n_biters_found <- rbinom(1,n_biters_investigated,pFound)
      n_biters_tested <- rbinom(1,n_biters_found,pTestable)
      
      # Get number of reactive vaccinations triggered this year
      reactive_vaccinations <- min(n_biters_tested,max_reactive_vaccinations)
      
      # Adjust vaccination this year based on reactive vaccinations
      vc_last_year_reactive[year+1] <- min(max_vc, vc_last_year[year+1] + (reactive_vaccinations*dogs_per_reactive_vax)/dogs) # assumes no revaccination
      
    }  
  }
  
  # get median and 95 percentile limits
  quantiles <- matrix(NA,nrow=3,ncol=years)
  for(year in 1:years){
    quantiles[,year] <- quantile(cases_mat[,year],c(0.025,0.5,0.975))
  }
  
  return(quantiles)
}


# Test
vc_last_year <- rep(0.2,8) # Try different underlying vaccination efforts
system.time({
  out_stoch <- stochastic(vc_last_year = vc_last_year)
  out_stoch_no_reactive <- stochastic(vc_last_year = vc_last_year,max_reactive_vaccinations = 0)
})

# Plot
par(mfrow=c(1,1))
plot(NA,ylim=c(0,max(out_stoch,out_stoch_no_reactive)),xlim=c(1,years),type="l",bty="l",ylab="Cases",xlab="Year")
polygon(c(1:years,rev(1:year)),c(out_stoch[1,],rev(out_stoch[3,])),col=scales::alpha("dodgerblue",0.3),border=NA, density = 10, angle = -45,lwd=4)
polygon(c(1:years,rev(1:year)),c(out_stoch_no_reactive[1,],rev(out_stoch_no_reactive[3,])),col=scales::alpha("orange",0.3),border=NA, density = 10, angle = 45,lwd=4)
lines(out_stoch_no_reactive[2,],lwd=2,col="darkorange",lty=2)
lines(out_stoch[2,],lwd=2,col="navy")




# Compare the approaches
#------------

# Run models for given vaccination regime
vc_last_year <- rep(0.2,8) # Try different underlying vaccination efforts
system.time(out_stoch <- stochastic(vc_last_year = vc_last_year,nreps = 10000))
system.time(out_semidet <- semideterministic(vc_last_year = vc_last_year,nreps = 10000))
# not a huge time difference

# Plot
par(mfrow=c(1,1))
plot(NA,ylim=c(0,max(out_stoch,out_semidet)),xlim=c(1,years),type="l",bty="l",ylab="Cases",xlab="Year")
polygon(c(1:years,rev(1:year)),c(out_stoch[1,],rev(out_stoch[3,])),col=scales::alpha("dodgerblue",0.3),border=NA, density = 10, angle = -45,lwd=4)
polygon(c(1:years,rev(1:year)),c(out_semidet[1,],rev(out_semidet[3,])),col=scales::alpha("orange",0.3),border=NA, density = 10, angle = 45,lwd=4)
lines(out_semidet[2,],lwd=2,col="darkorange",lty=2)
lines(out_stoch[2,],lwd=2,col="navy")
