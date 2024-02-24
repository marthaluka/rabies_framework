

# Predicting exposures that seek care using decision tree model
library(tidyverse)


## helper functions #######
source("./scripts/Helper_Fun_Gavi_forecasting.R")

## read parameters file #######
parameters_df <- read.csv("../ea_framework/data/parameters.csv")


# Find patients that seek care, given the parameter values
pts_seek <- run_decision_tree_and_select_variables2(scenario_name="no_interventions", 
                     parameters_df=parameters_df,HDR=c(25,35),
                     pop=350000,
                     selected_vars = c('ts_exposures_seek_care', 'ts_healthy_seek_care')
                     )

# sum the two (healthy and rabid bites that seek care)
pts_seek <- pts_seek$ts_exposures_seek_care +  pts_seek$ts_healthy_seek_care
pts_seek <- summarise_stochasticity(pts_seek)




# Run a series of different HDRs. Create a function
  # HDR list to use
#HDR_list <- list(c(5,10), c(15,25), c(35,50), c(65,75), c(85,100))
HDR_list <- list(c(2,4), c(4.1,8), c(8.1,16), c(16.1,32), c(32.1, 64), c(64.1,128))

  # Loop function through different HDRs
pts_seek_temp <- lapply(HDR_list, function(HDR) {
  pts_seek_temp <- run_decision_tree_and_select_variables2(scenario_name="no_interventions", 
                                                          pop=350000,
                                                          parameters_df=parameters_df, 
                                                          selected_vars = c('ts_exposures_seek_care', 'ts_healthy_seek_care'),
                                                          HDR = HDR
  )
  
  # sum the two (healthy and rabid bites that seek care)
  pts_seek_temp <- pts_seek_temp$ts_exposures_seek_care + pts_seek_temp$ts_healthy_seek_care

  #summarise_stochasticity(pts_seek_temp)
  
  # pts_seek_temp
  
})

pts_seek_list <- lapply(pts_seek_temp, function(matrix) { summarise_stochasticity(matrix) })

# extract monthly value from annual predictions
my_data<- extract_monthly(pts_seek_list, HDR_list)


## Bite patient presentations ########
monthly_bite_presentations_across_hdrs<- function(HDRs=HDR_list, scenario_name="no_interventions", pop=350000, parameters=parameters_df, 
                                                  selected_vars=c('ts_exposures_seek_care', 'ts_healthy_seek_care')){
  
  pts_seek_temp <- lapply(HDR_list, function(HDR) {
    pts_seek_temp <- run_decision_tree_and_select_variables2(scenario_name=scenario_name, 
                                                             pop=pop,
                                                             parameters=parameters_df, 
                                                             selected_vars = selected_vars,
                                                             HDR = HDR)
    # sum the two (healthy and rabid bites that seek care)
    pts_seek_temp <- pts_seek_temp$ts_exposures_seek_care + pts_seek_temp$ts_healthy_seek_care
    
  })
  
  # Extract the first column from each matrix in the list
  extracted_columns <- lapply(pts_seek_temp, function(x) x[,1])
  
  # Combine the extracted columns into a new matrix
  result_matrix <- ceiling(do.call(cbind, extracted_columns) / 12)  # monthly preds under diff HDRs
  return(result_matrix)
}

monthly_bite_presentations <- monthly_bite_presentations_across_hdrs()



## Predict vial use from bite presntation
## Assumptions:
    # Full compliance
    # All bite patients receive PEP (both rabid and healthy bites)


n_rows <- nrow(monthly_bite_presentations)
n_cols <- ncol(monthly_bite_presentations)

first_visits <- array(0, dim=c(n_rows, n_cols, 60))  # Assuming 60 days for each cell in the input matrix

for (i in 1:n_rows) {
  for (j in 1:n_cols) {
    total_patients <- monthly_bite_presentations[i, j]
    first_visits[i, j, ] <- distribute_patients_across_days(total_patients)
  }
}

# Note, first_visits is a 3D array (x=rows,  y=columns of monthly_bite_presentations), and z, the daily patient counts for 60 days)

# Adjust patient counts with repeat doses

# Define the regimen
regimen <- 'IPC'

# Initialize an empty array for adjusted results with the same dimensions as first_visits
adjusted_results <- array(0, dim = dim(first_visits))

# Loop through the dimensions of the original results
for (i in 1:dim(first_visits)[1]) {
  for (j in 1:dim(first_visits)[2]) {
    # Get the patient counts for the current category
    patient_counts <- first_visits[i, j, ]
    
    # Adjust patient counts based on the regimen
    adjusted_counts <- adjust_patient_counts(patient_counts, regimen)
    
    # Store the adjusted counts in the new 3D array
    adjusted_results[i, j, ] <- adjusted_counts
  }
}

# Now, adjusted_results contains the adjusted patient counts


# Now, adjusted_results contains the adjusted patient counts


# # IBCM data per clinic ########
# library(zoo)
# 
# 
# # IBCM data (compare with model predictions)
# patients_per_clinic <-  read.csv("./data/bites_pts_facility.csv") %>%
#   dplyr::mutate(Year_month = as.yearmon(Year_month)) %>%
#   #drop_na(new_FACILITY) %>%
#   group_by(new_FACILITY, Year_month) %>%
#   summarise(total_n = sum(n)) %>%
#   ungroup()
# 
# head(patients_per_clinic)
# 
# # medians
# median_bites<- patients_per_clinic %>%
#   group_by(new_FACILITY) %>%
#   filter(n() >= 5) %>%
#   summarise(median_patients = median(total_n, na.rm = TRUE))
# 
# 
# patients_per_clinic %>%
#   group_by(new_FACILITY) %>%
#   filter(n() >= 5) %>%
#   ungroup() %>%
#   ggplot(., aes(x=new_FACILITY, y=total_n)) +
#   geom_boxplot() +
#   labs(title="IBCM Bite patients per Month per Facility",
#        x= NULL,
#        y="Bite patients per month") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# # plot Monthly Bite patients per Health Center given different HDRs
# 
# HDR_string <- sapply(HDR_list, function(x) paste(x, collapse = "-"))
# my_data$HDR <- factor(my_data$HDR, levels = HDR_string)
# my_data$HDR_num <- 1:nrow(my_data)
# 
# manual_list <- c( 'Morogoro General Hospital','Kibaoni Health Center', 'Uhuru Health Center', 'RCH B Health Center', 
#                   'Dumila Dispensary')
# 
# manual_list2 <- c( 'Dumila Dispensary', 'Morogoro General Hospital',
#                    'Kibaoni Health Center', 'Uhuru Health Center',  'RCH B Health Center', '')
# 
# plot_data <- patients_per_clinic %>%
#   dplyr::filter(new_FACILITY %in% c('Dumila Dispensary', 'Morogoro General Hospital',
#                                     'Kibaoni Health Center', 'Uhuru Health Center',  'RCH B Health Center'))%>%
#   dplyr::mutate(HDR_num = match(new_FACILITY, manual_list) %% 5 + 1)
# 
# table(plot_data$HDR_num)
# 
# 
# my_data
# plot_data
# 
# 
# label_data <- plot_data %>%
#   group_by(new_FACILITY, HDR_num) %>%
#   summarize(max_total_n = max(total_n, na.rm = TRUE)) %>%
#   ungroup()
# 
# ggplot() +
#   geom_boxplot(data=plot_data, aes(x=HDR_num, y=total_n, group =new_FACILITY)) +
#   geom_ribbon(data= my_data, aes(x = rev(HDR_num), ymin = LL, ymax = UL), fill = "royalblue", alpha = 0.3) +
#   geom_line(data= my_data, aes(x = rev(HDR_num), y = Median), size=1, color = "royalblue") +
#   geom_text(data=label_data, aes(x=HDR_num, y=max_total_n + 2, label=new_FACILITY), vjust=0, angle=45, size=3) +  # Adjusting vjust and angle to ensure labels are clearly visible
#   labs(title = "Monthly Bite patients per Health Center given different HDRs",
#        y = "Bite patients",
#        x = "HDR") +
#   theme_bw() +
#   scale_x_continuous(breaks = my_data$HDR_num, labels = rev(my_data$HDR))
#   






# Vial use #########
            # ## Approach 1 ######
            # 
            # N=100
            # vials_per_dose <- runif(n=N, min = 0.1, max = 1)  #vials per dose
            # 
            # # probability that only 30% share a vial
            # vials_per_dose <- 1/rbinom(n = N, size = 5, prob = 0.3)
            # vials_per_dose <- ifelse(is.infinite(vials_per_dose), 1, vials_per_dose)
            # 
            # table(vials_per_dose)
            # 
            # total_PEP_intradermal <- vials_per_dose * patients_per_clinic
            # total_PEP_intramuscular <- Total_PEP_doses
            # 


## Approach 2 ######

# Simulate daily number of patients presenting to health center each day

# simulated monthly bite patients across HDRs
my_data2 <-data.frame(
  bite_pts = ceiling(my_data[,2]),
  HDR = my_data[,4]
)


# Test
distribute_patients_across_days(18)

# Run N times
N=100
patients_matrix <- t(replicate(N, distribute_patients_across_days(18)))

rowSums(patients_matrix)

# Run this across my_data2
# list to store the results 
patient_counts_list <- list()

# Loop through each scenario in my_data2
for(i in 1:nrow(my_data2)) {
  bite_pts_for_scenario <- my_data2$bite_pts[i]
  
  # Generate the patient distributions for the scenario and store in the results list
  patient_counts_list[[i]] <- t(replicate(N, distribute_patients_across_days(bite_pts_for_scenario)))
}

names(patient_counts_list) <- my_data2$HDR
patient_counts_list[[1]]


# Here, results is a list where each element is a matrix of dimensions `days_in_month` x `N`. 
# Each matrix corresponds to the distribution of patients across days for a given scenario.



# Using PEP regimens ##########
        # 
        # #Vaccination schedule and regimens current in use
        # schedule = c(0, 3, 7, 14, 21, 28, 90) # dates for delivery
        # 
        # # Rabies regimens
        # IPC = c(2,2,2,0,0,0,0)      # WHO recommended ID 
        # essen4 = c(1,1,1,1,0,0,0)     #IM Essen reduced 4 dose


              # # Adjust cases to account for repeat doses - number of doses defined by regimen
              # adjust_patient_counts <- function(patient_counts, regimen) {
              #   schedule <- c(0, 3, 7, 14)
              #   
              #   if (regimen == 'IPC') {
              #     doses = 3     
              #   } else if (regimen == 'essen4') {
              #     doses = 4
              #   } else {
              #     stop("Invalid regimen provided")
              #   }
              #   
              #   adj_patient_counts <- patient_counts
              #   days_in_month <- length(patient_counts)
              #   
              #   for (day in 1:days_in_month) {
              #     for (dose in 2:doses) {
              #       next_day <- day + schedule[dose]
              #       adj_patient_counts[next_day] <-  patient_counts[next_day] + adj_patient_counts[day]
              #       }
              #     }
              #   
              #   return(adj_patient_counts)
              # }




# Test the functions
total_patients <- 18
days_in_month <- 30

# reverting back to monthly numbers
patient_counts <- distribute_patients_across_days(total_patients, days_in_month)#[1:30]
adj_patient_counts_IPC <- adjust_patient_counts(patient_counts, "IPC")#[1:30]
adj_patient_counts_essen4 <- adjust_patient_counts(patient_counts, "essen4")#[1:30]
adj_patient_counts_essen5 <- adjust_patient_counts(patient_counts, "essen5")


patient_counts
adj_patient_counts_IPC
adj_patient_counts_essen4

# patient counts across regimens across HDRs
patient_counts_list
adj_patient_counts_IPC_list <- lapply(patient_counts_list, function(x) adjust_patient_counts(x, "IPC"))
adj_patient_counts_essen4_list <- lapply(patient_counts_list, function(x) adjust_patient_counts(x, "essen4"))
adj_patient_counts_essen5_list <- lapply(patient_counts_list, function(x) adjust_patient_counts(x, "essen5"))


# Vaccine vials across regimens across HDRs

# Vials used (assuming availability is no issue and all seekers start PEP)
calculate_vials_used <- function(patients_matrix, PEP_regimen, Vial_size) {
  if(PEP_regimen == "ID" & Vial_size == 1){
    # ID with 1 ml vial
    vials_matrix <- ceiling(patients_matrix / 5)  # up to 5 patients per vial in ID
  } else if (PEP_regimen == "ID" & Vial_size == 0.5){
    # ID with 0.5 ml vial
    vials_matrix <- ceiling(patients_matrix / 2)  # up to 2 patients per vial in ID with 0.5 ml 
  } else {
    # IM (size doesn't matter)
    vials_matrix <- patients_matrix
  }
  return(vials_matrix)
}

# run
vials_matrix <- calculate_vials_used(patients_matrix, "ID", 0.5)
vials_matrix
vial_use <- rowSums(vials_matrix)

quantile(vial_use, probs = c(0.025, 0.5, 0.975))

calculate_vials_used(adj_patient_counts_IPC, "ID", 0.5)

 
# Vials across regimens HDRs
# vials_IPC_list <- lapply(adj_patient_counts_IPC_list, function(x) calculate_vials_used(x, "IPC"))
# vials_essen4_list <- lapply(patient_counts_list, function(x) calculate_vials_used(x, "essen4"))

IPC_0.5ml_list <- lapply(adj_patient_counts_IPC_list, function(x) calculate_vials_used(x,PEP_regimen="ID", Vial_size=0.5))

IPC_1ml_list  <- lapply(adj_patient_counts_IPC_list, function(x) calculate_vials_used(x,PEP_regimen="ID", Vial_size=1))

essen4_list  <- lapply(adj_patient_counts_essen4_list, function(x) calculate_vials_used(x,PEP_regimen="IM", Vial_size=0.5))

essen5_list  <- lapply(adj_patient_counts_essen5_list, function(x) calculate_vials_used(x,PEP_regimen="IM", Vial_size=0.5))




# summarise clinic presentations given the different doses (assuming full compliance)
IPC_clinic_presentations <- calculate_quantiles(adj_patient_counts_IPC_list)
essen4__clinic_presentations <- calculate_quantiles(adj_patient_counts_essen4_list)
essen5__clinic_presentations <- calculate_quantiles(adj_patient_counts_essen5_list)


# summarise vials given the different doses (assuming full compliance)

IPC_0.5ml<- calculate_quantiles(IPC_0.5ml_list) %>%
  dplyr::mutate(
    regimen="IPC_0.5ml",
    HDR_num = seq(1,length(IPC_clinic_presentations$HDR)), 
    bite_pts = IPC_clinic_presentations$Median,
    vials_per_pt = Median/bite_pts,
    vials_per_pt_LL = LL/IPC_clinic_presentations$LL,
    vials_per_pt_UL = UL/IPC_clinic_presentations$UL
  )

IPC_1ml<- calculate_quantiles(IPC_1ml_list) %>%
  dplyr::mutate(
    regimen="IPC_1ml",
    HDR_num = seq(1,length(IPC_clinic_presentations$HDR)),
    bite_pts = IPC_clinic_presentations$Median,
    vials_per_pt = Median/bite_pts,
    vials_per_pt_LL = LL/IPC_clinic_presentations$LL,
    vials_per_pt_UL = UL/IPC_clinic_presentations$UL
  )

essen4 <- calculate_quantiles(essen4_list ) %>%
  dplyr::mutate(
    regimen="essen4",
    HDR_num = seq(1,length(essen4__clinic_presentations$HDR)),
    bite_pts = essen4__clinic_presentations$Median,
    vials_per_pt = Median/bite_pts,
    vials_per_pt_LL = LL/essen4__clinic_presentations$LL,
    vials_per_pt_UL = UL/essen4__clinic_presentations$UL
  )

essen5 <- calculate_quantiles(essen4_list ) %>%
  dplyr::mutate(
    regimen="essen5",
    HDR_num = seq(1,length(essen5__clinic_presentations$HDR)),
    bite_pts = essen5__clinic_presentations$Median,
    vials_per_pt = Median/bite_pts,
    vials_per_pt_LL = LL/essen5__clinic_presentations$LL,
    vials_per_pt_UL = UL/essen5__clinic_presentations$UL
  )

combined <- bind_rows(IPC_0.5ml, IPC_1ml, essen4, essen5)

## Plot
library(RColorBrewer)

ggplot() +
  geom_ribbon(data= combined, aes(x = rev(HDR_num), ymin = LL, ymax = UL, fill= regimen), alpha = 0.5) +
  geom_line(data = combined, aes(x = rev(HDR_num), y = Median, color= regimen)) +
  labs(title = "Vials used per Month given different regimens/  HDRs",
       y = "Total vials",
       x = "HDR") +
  theme_bw() +
  scale_x_continuous(breaks = combined$HDR_num, labels = rev(combined$HDR)) +
  scale_fill_brewer(palette="Set2") + 
  scale_color_brewer(palette="Set2")


ggplot() +
  geom_ribbon(data= combined, aes(x = rev(HDR_num), ymin = vials_per_pt_LL, ymax = vials_per_pt_UL, fill= regimen), alpha = 0.5) +
  geom_line(data = combined, aes(x = rev(HDR_num), y = vials_per_pt, color= regimen)) +
  labs(title = "Vials per patient",
       y = "Vials per patient",
       x = "HDR") +
  theme_bw() +
  scale_x_continuous(breaks = combined$HDR_num, labels = rev(combined$HDR)) +
  scale_fill_brewer(palette="Set2") + 
  scale_color_brewer(palette="Set2")









# Visualise



          # ## No Gavi vs Gavi (free PEP) ###########
          # 
          # ts_healthy_start<- compare_scenarios("ts_healthy_start", no_interventions, PEP_ID_free_only)
          # ts_healthy_start$LL <- as.numeric(as.character(ts_healthy_start$LL))
          # ts_healthy_start$Median <- as.numeric(as.character(ts_healthy_start$Median))
          # ts_healthy_start$UL <- as.numeric(as.character(ts_healthy_start$UL))
          # ts_healthy_start[, c("LL", "Median", "UL")] <- ts_healthy_start[, c("LL", "Median", "UL")] / 7
          # ts_healthy_start
          # 
          # ts_exp_start<- compare_scenarios("ts_exp_start", no_interventions, PEP_ID_free_only)
          # ts_exp_start$LL <- as.numeric(as.character(ts_exp_start$LL))
          # ts_exp_start$Median <- as.numeric(as.character(ts_exp_start$Median))
          # ts_exp_start$UL <- as.numeric(as.character(ts_exp_start$UL))
          # ts_exp_start[, c("LL", "Median", "UL")] <- ts_exp_start[, c("LL", "Median", "UL")] / 7
          # ts_exp_start

# Katie's code #########

#Vaccination schedule and regimens current in use
schedule = c(0, 3, 7, 14, 21, 28, 90) # dates for delivery

# Rabies regimens
IPC = c(2,2,2,0,0,0,0)      # WHO recommended ID 
essen4 = c(1,1,1,1,0,0,0)     #IM Essen reduced 4 dose

# All different regimens combined (9)
regimens = list(IPC = c(2,2,2,0,0,0,0), # WHO recommended ID
              essen4 = c(1,1,1,1,0,0,0) #IM Essen reduced 4 dose
              )

# doses per vial wasted & not wasted (0.5ml vial and 1 ml vial)
v = data.frame(regimen = c("IPC", "essen4"))
v$vdoses_nowaste = c(4,1)
v$vdoses_nowaste1 = c(10,1)

#look at different numbers of vials used for different regimens
patients=c(1,5,10,20,30,40,50,75,100,500) # New patients per month
reps =  1000


# VACCINE USE ACCORDING TO PATIENT COMPLIANCE
#####################################################################
# CONSIDER SPECIFYING COMPLIANCE FOR EACH SPECIFIC DOSE FOR
# SPECIFIC REGIMENS BASED ON DATA
# E.G. TRC4, d1-4: c(1, 0.9, 0.3, 01)
# ALSO CONSIDER COMPARING DATES OF PATIENT VISITS WITH RANDOM UNIFORM
# SAMPLING VS CLUSTERED SAMPLING i.e. patients clustering together in
# time. Need to check poisson spatial sampling model, where for a subset
# of patients,
#####################################################################
VusePC=function(pc, sdate, regimen=TRC4){
  # list days when each vaccination is delivered according to the probability of compliance (pc)
  last=NA
  finish=which(rbinom(4,1,pc)==0)[1]       # end when first visit is abandoned!
  last=ifelse(is.na(finish), 6, finish+1)  # chose a long enough sequence
  days=schedule[which(regimen!=0)][1:last]
  doses=regimen[which(regimen!=0)][1:last]
  rep(days[!is.na(days)], doses[!is.na(doses)])+sdate #work out from the startdate!
}
# VusePC(0.5, 5) #examples - set.seed(50); startdates=floor(sort(runif(10*12, 1, 365)))



#####################################################################
# VIAL COUNT BASED ON HOSPITAL VISITS & DOSES PER VIAL
vial.count=function(visits, doses.per.vial){
  # calculate no. vials used based on patient reporting dates
  # requires days of patient visits and assumption about doses/vial,
  # default=5 (i.e. 0.5mL vial, each dose=0.1mL)
  vcount=table(visits)
  max.vials=ceiling(max(vcount)/doses.per.vial) #max no. vials used in 1 day
  sum(hist(vcount, breaks=seq(0, max.vials*doses.per.vial, by=doses.per.vial), plot=FALSE)$counts * (1:max.vials)) #vials used
}



#####################################################################
# CALCULATE VIAL USE UNDER DIFFERENT REGIMENS, THRU-PUT, COMPLIANCE & DOSES/VIAL
eval.vials=function(m.patients, doses.per.vial=5, regimen=IPC, pc=1){
  # DEFAULT=TRC, 5 inj/vial and 100% compliance
  # m.patients = monthly NEW bite patients!
  startdates=floor(sort(runif(m.patients*12, 1, 365)))          # Start PEP dates for patients (over year)
  vaccdates=unlist(lapply(startdates, VusePC, pc=pc, regimen=regimen))  #d elivery dates for injections
  vial.count(vaccdates[which(vaccdates>31)], doses.per.vial)
  # cut last line because make proportion of vials used >1 sometimes....
  #  vial.count(vaccdates, doses.per.vial)
  # last line means counting vials after first month
}

#examples
# eval.vials(m.patients=12); eval.vials(12, 1, essen4)
# # eval.vials(m.patients=12, 5, TRC, pc=0.5)


# 1. 100% compliance, 0.5ml vials, perfect usage
for(j in 1: length(regimens)){
  vials = matrix(nrow=length(patients), ncol=5)
  fname = paste("./figures/v", names(regimens)[j], ".csv", sep="")
  print(fname)
  
  # Go thru regimens
  for(i in 1:length(patients)){ # 500 simulations
    vial.res=replicate(reps, eval.vials(m.patients=patients[i], doses.per.vial=v$vdoses_nowaste[j], unlist(regimens[j]), pc=1))
    vials[i, ]=quantile(vial.res, c(0.01, 0.05, 0.5, 0.95, 0.99))
    print(i)
  }
  
  #return(vials)
  write.csv(vials, file=fname, row.names=FALSE) # write to csv
}






             
             
             
             