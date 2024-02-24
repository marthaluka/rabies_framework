
require(pacman)
pacman::p_load(tidyverse, # cleaning, wrangling
               sf, # spatial manipulation
               leaflet, # leaflet maps
               shiny, # interactive web apps
               shinycssloaders, # loading symbol for app
               RColorBrewer, # color palettes
               htmltools,  # HTML generation and tools
               scales, # format numbers for aesthetics
               patchwork,  # multiple plots
               data.table,# data tables for memory efficiency
               reshape2,
               DT   # rendering interactive tables on app
)

# read shapefile #####
east_africa_shp <- st_read(dsn="./shapefiles/", layer="ea_shapefile")
#east_africa_shp <- east_africa_shp[-c(135, 117, 153), ]

east_africa_shp$Population <- as.numeric(east_africa_shp$Population) 
east_africa_shp$county_id <- rownames(east_africa_shp)

# read prediction data 
predictions <- fread("./data/combined_predictions.csv")

## Scenarios table
## read parameters file
scenarios_df <- read.csv("./data/parameters.csv") %>%
  dplyr::mutate(
    scenario_id = seq(1, length(scenario)),
    shiny_app_option1 = c("Status quo", "PEP free", "PEP free", "MDV", "MDV and PEP free", "MDV and PEP free"),
    shiny_app_option2 = c("", "Intramuscular", "Intradermal", "", "Intramuscular", "Intradermal")
    ) %>%
  dplyr::select(scenario, scenario_id, shiny_app_option1, shiny_app_option2) 
  

scenarios_df

# for app --to review this
# countries in shapefile
countries <- sort(unique(east_africa_shp$Country))

# variables for selection. To review
variables <- c("ts_complete_PEP","ts_cost_PEP_per_year","ts_cost_per_year", "ts_deaths", "ts_deaths_averted_PEP",   
               "ts_deaths_no_PEP", "ts_dogs_vaccinated", "ts_exp_complete", "ts_exp_incomplete",  "ts_exp_no_start",   
               "ts_exp_start","ts_exposures", "ts_exposures_do_not_seek_care", "ts_exposures_seek_care",   "ts_exposures_seek_care_inc",   
               "ts_healthy_bites",  "ts_healthy_complete", "ts_healthy_do_not_seek_care",   "ts_healthy_FP", "ts_healthy_incomplete",   
               "ts_healthy_seek_care", "ts_healthy_seek_care_inc", "ts_incomplete_PEP", "ts_MDV_campaign_cost","ts_rabid_biting_dogs",    
               "ts_rabid_biting_found", "ts_rabid_biting_investigated",  "ts_rabid_biting_testable", "ts_rabid_dogs"  )




# Function to filter predictions table

filter_and_transform_data <- function(selected_variable, scenario_option1, scenario_option2, country#, horizon
                                      ) {
  
  # Filter by variable
  filtered_data <- subset(predictions, variable == selected_variable)
  
  # Filter by scenario
  scenario_id <- scenarios_df[scenarios_df$shiny_app_option1 == scenario_option1 & 
                                scenarios_df$shiny_app_option2 == scenario_option2, 
                              "scenario_id"]
  filtered_data <- subset(filtered_data, scenario == scenario_id)
  
  # Filter by country
  county_ids <- east_africa_shp[east_africa_shp$Country == country,]$county_id
  filtered_data <- subset(filtered_data, county_id %in% county_ids)
  
  # Drop unnecessary columns and round Median
  filtered_data$LL <- NULL
  filtered_data$UL <- NULL
  filtered_data$Median <- round(filtered_data$Median)
  
  # Filter by horizon
  #filtered_data <- subset(filtered_data, year <= horizon)
  
  # Replace county_id with County name
  county_dict <- setNames(east_africa_shp$County, east_africa_shp$county_id)
  filtered_data$County <- county_dict[filtered_data$county_id]
  #filtered_data$county_id <- NULL
  filtered_data$scenario <- NULL
 
  
  # Transform to wide format
  wide_data <- reshape2::dcast(filtered_data, County ~ year, value.var = "Median")
  
  return(wide_data)
}


filter_and_transform_data("ts_complete_PEP", "Status quo", "", "Tanzania")




