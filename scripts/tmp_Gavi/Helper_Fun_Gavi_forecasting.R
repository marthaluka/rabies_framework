


# Run model to extract variable

# source model
source("./scripts/stochastic_decision_tree.R")

## read parameters file
parameters_df <- read.csv("./data/parameters.csv")

# run model, selecting only variables of interest
run_decision_tree_and_select_variables2 <- function(scenario_name, parameters_df, pop=350000, horizon = 2, base_vax_cov=0.05, 
                                                    N = 1000, HDR, selected_vars=NULL){
  scenario_parameters <- parameters_df[parameters_df$scenario == scenario_name, ]
  
  result <- decision_tree(
    N = N,
    pop = pop,
    horizon = horizon, 
    base_vax_cov=base_vax_cov,
    discount = scenario_parameters$discount,
    target_vax_cov = scenario_parameters$target_vax_cov,
    HDR = HDR,
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
summarise_stochasticity <-  function(my_matrix){
  out<- apply(my_matrix, 2, quantile, c(0.025, 0.5, 0.975), na.rm=TRUE)
  rownames(out)<-NULL
  return(out)
}


# extract monthly value from annual predictions
extract_monthly <- function(matrix_list, HDR_list) {
  # Extract only the first year
  first_column_list <- lapply(matrix_list, function(x) {
    x[, 1]
  })
  
  # Get monthly value from annual prediction
  result_df <- as.data.frame(do.call(rbind, first_column_list) / 12)
  names(result_df) <- c('LL', 'Median', 'UL')
  
  # Label with respective HDR
  HDR_string <- sapply(HDR_list, function(x) paste(x, collapse = "-"))
  result_df$HDR <- HDR_string
  
  return(result_df)
}


# Using monthly bite patient numbers, calculate daily patient presentations for NEW patients
distribute_patients_across_days <- function(total_patients, days_under_check = 30) { 
  # using a length of 60 days to avoid NAs as we calculate repeat doses.
  # This is later taken care of to revert back to monthly numbers
  total_patients= total_patients*2
  days_under_check = days_under_check*2 #(I dont think this is necessary any more)
  
  # Randomly assign each patient to a day
  assigned_days <- sample(1:days_under_check, total_patients, replace = TRUE)
  
  # Count the occurrences of each day to get the no of patients presenting on that day
  counts <- table(assigned_days)
  
  # Create a full vector to account for days when no patients might be assigned
  patient_counts <- integer(days_under_check)
  patient_counts[as.integer(names(counts))] <- counts
  
  return(patient_counts)
}


# Adjust cases to account for repeat doses - number of doses defined by regimen
# Regimens allowed: c('IPC', 'essen4', 'essen5')
adjust_patient_counts <- function(patient_counts, regimen) {
  schedule <- c(0, 3, 7, 14, 28)
  
  adj_patient_counts <- patient_counts
  
  if (regimen == 'IPC') {
    for (i in 1:length(patient_counts)) {
      # Check if the i+3 position exists in the list
      if ((i + 3) <= length(patient_counts)) {
        adj_patient_counts[i + 3] <- adj_patient_counts[i + 3] + patient_counts[i]
      }
      # Check if the i+7 position exists in the list
      if ((i + 7) <= length(patient_counts)) {
        adj_patient_counts[i + 7] <- adj_patient_counts[i + 7] + patient_counts[i]
      }
    }
    
  } else if (regimen == 'essen4') {
    for (i in 1:length(patient_counts)) {
      # Check if the i+3 position exists in the list
      if ((i + 3) <= length(patient_counts)) {
        adj_patient_counts[i + 3] <- adj_patient_counts[i + 3] + patient_counts[i]
      }
      # Check if the i+7 position exists in the list
      if ((i + 7) <= length(patient_counts)) {
        adj_patient_counts[i + 7] <- adj_patient_counts[i + 7] + patient_counts[i]
      }
      if ((i + 14) <= length(patient_counts)) {
        adj_patient_counts[i + 14] <- adj_patient_counts[i + 14] + patient_counts[i]
      }
    }
    
  } else if (regimen == 'essen5') {
    for (i in 1:length(patient_counts)) {
      # Check if the i+3 position exists in the list
      if ((i + 3) <= length(patient_counts)) {
        adj_patient_counts[i + 3] <- adj_patient_counts[i + 3] + patient_counts[i]
      }
      # Check if the i+7 position exists in the list
      if ((i + 7) <= length(patient_counts)) {
        adj_patient_counts[i + 7] <- adj_patient_counts[i + 7] + patient_counts[i]
      }
      if ((i + 14) <= length(patient_counts)) {
        adj_patient_counts[i + 14] <- adj_patient_counts[i + 14] + patient_counts[i]
      }
      if ((i + 28) <= length(patient_counts)) {
        adj_patient_counts[i + 28] <- adj_patient_counts[i + 28] + patient_counts[i]
      }
    }
    
    
  } else {
    stop("Invalid regimen provided")
  }
  
  return(adj_patient_counts)
}



# Vials used (assuming availability is no issue and all seekers get PEP)
# Vials are either 1ml or 0.5 ml
# PEP_admin either 'IM' or 'ID'
calculate_vials_used <- function(patients_matrix, PEP_admin, Vial_size) {
  Vial_size <- as.numeric(Vial_size)
  
  # Validate arguments provided
  if (!(PEP_admin %in% c("ID", "IM")) | !(Vial_size %in% c(0.5, 1))) {
    stop("Invalid PEP admin or Vial size")
  }
  
  if(PEP_admin == "ID" & Vial_size == 1){
    # ID with 1 ml vial
    vials_matrix <- ceiling(patients_matrix / 5)  # up to 5 patients per vial in ID
  } else if (PEP_admin == "ID" & Vial_size == 0.5){
    # ID with 0.5 ml vial
    vials_matrix <- ceiling(patients_matrix / 2)  # up to 2 patients per vial in ID with 0.5 ml 
  } else {
    # IM (size doesn't matter)
    vials_matrix <- patients_matrix
  }
  return(vials_matrix)
}



# summarise
calculate_quantiles <- function(adj_patient_counts_list) {
  # Slicing matrix to the first 30 columns
  sliced_matrices_list <- lapply(adj_patient_counts_list, function(matrix) {
    return(matrix[, 1:30])
  })
  
  quantiles_list <- lapply(sliced_matrices_list, function(matrix) {
    row_sums <- rowSums(matrix)
    return(quantile(row_sums, probs = c(0.025, 0.5, 0.975)))
  })
  
  # names
  HDR <- names(quantiles_list)
  df <- do.call(rbind, lapply(quantiles_list, function(x) as.data.frame(t(as.matrix(x)))))
  df$HDR <- HDR
  
  # Ordering columns
  df <- df[, c("HDR", "2.5%", "50%", "97.5%")]
  rownames(df) <- NULL
  names(df)<- c("HDR", "LL", "Median", "UL")
  
  return(df)
  
}
