

# libraries
require(pacman)
pacman::p_load(tidyverse, # cleaning, wrangling
               sf, # spatial manipulation
               leaflet # leaflet maps
)


# Read old EA shapefile
old_EA_shapefile <- st_read(dsn="./shapefiles/old_EA_shapefile/", layer="ea_shapefile") %>%
  dplyr::filter(Country != "Tanzania")


# TZ shape file -- this uses older district names and is compatible with IBCM data
TZ_shapefile <- st_read(dsn="./shapefiles/TZ_shapefile_Ellie/", layer="tz_districts_2022") %>%
  dplyr::mutate(
    Country = "Tanzania",
    County = dist_nm,
    Population = Popultn
  ) %>%
  dplyr::select(
    County, Country, Population,
  )

# Merge old_EA_shapefile (without Tanzania) with new TZ_shapefile
new_EA_shapefile <- bind_rows(old_EA_shapefile, TZ_shapefile)

# Write to file
st_write(new_EA_shapefile, "./shapefiles/ea_shapefile/new_EA_shapefile.shp", delete_layer = TRUE)













