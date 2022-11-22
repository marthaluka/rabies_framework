


# libraries
require(pacman)
pacman::p_load(tidyverse, # cleaning, wrangling
               sf, # spatial manipulation
               leaflet, # leaflet maps
               shiny, # interactive web apps
               shinycssloaders, # loading symbol for app
               RColorBrewer, # color palettes
               htmltools, # HTML generation and tools
               r2d3maps, # D3 maps
               here)



setwd(here::here())
east_africa_shp <- st_read(dsn="./shapefiles/", 
                           layer="ea_shapefile")

east_africa_shp$Population <- as.numeric(east_africa_shp$Population)

str(east_africa_shp)

source("./scripts/deterministic_model.R")

df1 <- deterministic_decision_tree(pop = east_africa_shp$Population, 
                        HDR=30,
                        vax_cov=0,
                        incidence=0.01,
                        P_bite_rabid=0.38,
                        P_bite_healthy= 0.2,
                        P_get_PEP_rabid_bite=0.3, 
                        P_get_PEP_healthy_bite=0.1,
                        P_death=0.9, 
                        P_prevent=0.98
                        )

east_africa_shp2 <- cbind(east_africa_shp, df1)

# color palette 
pal <- 
  colorBin(
    palette = "YlOrRd",
      domain = east_africa_shp2$Population)

# pop up message
labels <- 
  sprintf(
    "<strong>%s</strong><br/>%g",
    east_africa_shp2$County, east_africa_shp2$Population) %>% 
  lapply(htmltools::HTML)


shinyApp(
  ui <- navbarPage("Rabies in East Africa", id="nav", 
                   
                   tabPanel(
                     "Interactive map",
                     withSpinner(leafletOutput(
                       outputId = "mymap", 
                       width = "900px", 
                       height = "500px"))),
                   
                   tabPanel("Explore the data",
                            DT::dataTableOutput("table"))
  ),
  
  server <- function(input, output) {
    
    # map panel 
    output$mymap <- renderLeaflet({
      
      # passing the shp df to leaflet
      leaflet(east_africa_shp2) %>%
        # zooming in on Kenya 
        setView(37.9062, 0.0236, 5) %>%
        # adding tiles, without labels to minimize clutter
        #addProviderTiles("CartoDB.PositronNoLabels") %>%
        
        # parameters for the polygons
        addPolygons(
          fillColor = ~pal(Population), 
          weight = 1,
          opacity = 1,
          color = "white",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal"),
            textsize = "15px",
            direction = "auto")) %>%
        # legend
        addLegend(pal = pal,
                  values = east_africa_shp2$Population,
                  position = "bottomright",
                  title = "Population",
                  opacity = 0.8,
                  na.label = "No data")
    })

    # data panel
    output$table <- DT::renderDataTable({
      DT::datatable(east_africa_shp2 %>% st_drop_geometry(), rownames = F,  filter = 'top',
                    extensions = c('Buttons', 'FixedHeader', 'Scroller'),
                    options = list(pageLength = 15, lengthChange = F,
                                   fixedHeader = TRUE,
                                   dom = 'lfBrtip',
                                   list('copy', 'print', list(
                                     extend = 'collection',
                                     buttons = c('csv', 'excel', 'pdf'),
                                     text = 'Download'
                                   ))
                    ))
    })
    
  }
  
  ,
  
  options = list(height = 700)
)









