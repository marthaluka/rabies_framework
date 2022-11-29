


# libraries
require(pacman)
pacman::p_load(tidyverse, # cleaning, wrangling
               sf, # spatial manipulation
               leaflet, # leaflet maps
               shiny, # interactive web apps
               shinycssloaders, # loading symbol for app
               RColorBrewer, # color palettes
               htmltools, # HTML generation and tools
               scales, # format numbers for aesthetics
               patchwork, # multiple plots
               here)



setwd(here::here())
east_africa_shp <- st_read(dsn="./shapefiles/", 
                           layer="ea_shapefile")

east_africa_shp$Population <- as.numeric(east_africa_shp$Population)

str(east_africa_shp)


source("./scripts/deterministic_model.R")

# model output with zero vaccination coverage
vax_coverages <- deterministic_decision_tree(pop = east_africa_shp2$Population, 
                                             HDR=30,   # varies from 4 - in Tz - Changalucha et al 2019
                                             vax_cov=0,
                                             incidence=0.01,    # incidence with no interventions in place
                                             P_bite_rabid=0.38,  # p=0.375
                                             P_bite_healthy= 0.2,   #to be confirmed (Tz data)
                                             P_get_PEP_rabid_bite=0.6, # paying for PEP (0.25 did not seek, 0.15 did not initiate)- proportion seek PEP is higher in urban centers, 
                                                                          # free PEP P_get_PEP_rabid_bite = 0.87-0.9 -- Changalucha et al 2019
                                             P_get_PEP_healthy_bite=0.5, #to be confirmed (Tz data)
                                             P_death=0.17,  # 0.165 -  Changalucha et al 2019
                                             P_prevent=0.99) # complete PEP (0.99-1) incomplete PEP (0.98-0.99) -- Changalucha et al 2019

#probability complete PEP (0.473 - 0.542) three doses

vax_coverages$vax_cov <- 0
vax_coverages <- cbind(east_africa_shp, vax_coverages)

# iterate through different possible vaccination coverage. 
  # We select a step of 0.1 so as to calculate this before shiny step (shiny just queries the df panel)
allowed_vax_covs <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)

# loop through different vax coverages
for (vax_cov in allowed_vax_covs){
  model_output<- deterministic_decision_tree(pop = east_africa_shp2$Population, 
                                             HDR=30,
                                             vax_cov=vax_cov,
                                             incidence=0.01,
                                             P_bite_rabid=0.38,
                                             P_bite_healthy= 0.2,
                                             P_get_PEP_rabid_bite=0.3, 
                                             P_get_PEP_healthy_bite=0.1,
                                             P_death=0.9, 
                                             P_prevent=0.98
  )
  model_output$vax_cov <- vax_cov
  model_output <- cbind(east_africa_shp, model_output)
  vax_coverages <- rbind(vax_coverages, model_output)
}

east_africa_shp2 <- vax_coverages



# countries in shapefile
countries <- sort(unique(east_africa_shp$Country))

# variables for selection. Can add or reduce
variables <- c("Population", "dog_population", "rabid_dogs", "total_rabid_bites", "total_healthy_bites", 
               "total_people_PEP", "rabies_deaths", "cost_per_life_saved")




# ui #####

ui <- fluidPage(
  
  titlePanel("Rabies in East Africa"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId="Country", label="Select a country:", choices = countries),
      sliderInput(inputId="vax_cov", label= "Dog vaccination coverage:", min=0, max=1, value=0, step = 0.1),
      selectInput(inputId="variable", label="Select a variable:", choices = variables),
      selectInput(inputId="PEP_policy", label="Policy choices for PEP:", choices = c("Offered under status quo", "Offered free of charge")),
      selectInput(inputId="PEP_admin", label="Method of PEP administration:", choices = c("Intramuscular", "Intradermal")),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Interactive map",
          withSpinner(leafletOutput(
            outputId = "mymap", 
            width = "900px", 
            height = "500px"))),
        
        tabPanel("Visualize",
                 plotOutput("plot")), 
        
        tabPanel("Explore the data",
                 DT::dataTableOutput("table")),
        
      
        tabPanel("About", verbatimTextOutput("summary")), 
        
      )
    )
  )
)
  


# server #####

server <- function(input, output) {

    # map panel 
    output$mymap <- renderLeaflet({
      
      # filter out selected country
      selected_country<- east_africa_shp2 %>%
        dplyr::filter(Country == input$Country,
                      vax_cov == input$vax_cov)
      
      #PEP admin route (impacts costs - and possibly no. of people who receive PEP-as fewer shortages- though latter not yet incorporated)
      if (input$PEP_admin == "Intradermal"){
        selected_country <- selected_country %>%
          dplyr::select(-c(total_PEP_intramuscular, cost_per_life_saved_intramuscular)) %>%
          dplyr::rename("Total PEP" = total_PEP_intradermal,
                        "cost_per_life_saved" = cost_per_life_saved_intradermal)
      } else { #default intramuscular
        selected_country <- selected_country %>%
          dplyr::select(-c(cost_per_life_saved_intradermal, cost_per_life_saved_intradermal)) %>%
          dplyr::rename("Total PEP" = total_PEP_intramuscular,
                        "cost_per_life_saved" = cost_per_life_saved_intramuscular)
      }
      
      
      # color palette
      pal <-
        colorBin(
          palette = "YlOrRd",
          domain = selected_country[[input$variable]])
      
      # pop up message
      labels <- 
        sprintf(
          "<strong>%s</strong><br/>%s",
          selected_country$County, scales::comma(selected_country[[input$variable]])) %>% 
        lapply(htmltools::HTML)
      
      # passing the shp df to leaflet
      leaflet(selected_country) %>%
        
        # adding tiles, without labels to minimize clutter
        #addProviderTiles("CartoDB.PositronNoLabels") %>%
        
        # parameters for the polygons
        addPolygons(
          fillColor = ~pal(eval(as.symbol(input$variable))), 
          weight = 1,
          opacity = 1,
          color = "white",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.8,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal"),
            textsize = "15px",
            direction = "auto")) %>%
        # legend
        addLegend(pal = pal,
                  values = selected_country[[input$variable]],
                  position = "bottomright",
                  title = input$variable,
                  opacity = 0.8,
                  na.label = "No data")
    })
    
    
    # visualize data 
    
    output$plot <- renderPlot({
      
      #transform the character name into a symbol
      select_col<- sym(input$variable)
      
      # filter out selected country
      selected_country<- east_africa_shp2 %>%
        dplyr::filter(Country == input$Country,
                      vax_cov == input$vax_cov)
      
      #PEP admin route (impacts costs - and possibly no. of people who receive PEP-as fewer shortages- though latter not yet incorporated)
      if (input$PEP_admin == "Intradermal"){
        selected_country <- selected_country %>%
          dplyr::select(-c(total_PEP_intramuscular, cost_per_life_saved_intramuscular)) %>%
          dplyr::rename("Total PEP" = total_PEP_intradermal,
                        "cost_per_life_saved" = cost_per_life_saved_intradermal)
      } else { #default intramuscular
        selected_country <- selected_country %>%
          dplyr::select(-c(cost_per_life_saved_intradermal, cost_per_life_saved_intradermal)) %>%
          dplyr::rename("Total PEP" = total_PEP_intramuscular,
                        "cost_per_life_saved" = cost_per_life_saved_intramuscular)
      }
      
      
      selected_country %>%
        #dplyr::arrange(!! select_col)%>%
        # use symbol unquoting with double exclamation mark !!
        ggplot(., aes(x=reorder(County, !! select_col), y= !! select_col))+
        geom_bar(stat="identity", fill="steelblue")+
        coord_flip() +
        theme_bw() +
        ggtitle(paste0("Total ", input$variable, " by county/district"))
      

    })
    
    
    

    # data panel
    output$table <- DT::renderDataTable({
      
      # filter out selected country
      selected_country<- east_africa_shp2 %>%
        dplyr::filter(Country == input$Country,
                      vax_cov == input$vax_cov)
      
      #PEP admin route (impacts costs - and possibly no. of people who receive PEP-as fewer shortages- though latter not yet incorporated)
      if (input$PEP_admin == "Intradermal"){
        selected_country <- selected_country %>%
          dplyr::select(-c(total_PEP_intramuscular, cost_per_life_saved_intramuscular)) %>%
          dplyr::rename("Total PEP" = total_PEP_intradermal,
                        "cost_per_life_saved" = cost_per_life_saved_intradermal)
      } else { #default intramuscular
        selected_country <- selected_country %>%
          dplyr::select(-c(cost_per_life_saved_intradermal, cost_per_life_saved_intradermal)) %>%
          dplyr::rename("Total PEP" = total_PEP_intramuscular,
                        "cost_per_life_saved" = cost_per_life_saved_intramuscular)
      }
      
      
      
      DT::datatable(selected_country %>% st_drop_geometry() %>%
                    dplyr::filter(Country == input$Country,
                                  vax_cov == input$vax_cov) %>% 
                    dplyr::select(c("Country","County", "Population", "dog_population", "rabid_dogs", "total_rabid_bites", "total_healthy_bites", 
                                    "total_people_PEP", "rabies_deaths", "cost_per_life_saved")), 
                    rownames = F,  filter = 'top',
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
  
  


shinyApp(ui, server, options = list(height = 700))



# Bug - labels get mismatched after filtering countries ---resolved


