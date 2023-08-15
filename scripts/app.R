

# countries in shapefile
countries <- sort(unique(east_africa_shp$Country))

# variables for selection. Can add or reduce
variables <- c("Population", "dog_population", "rabid_dogs", "total_rabid_bites", 
               "total_people_PEP", "rabies_deaths", "lives_saved", "cost_per_life_saved")


### in app.R
app <- shinyApp(ui, server, onStart = test())

runApp(app)

