

# Pre-run the two scripts before launching the app

# source("./scripts/run_models.R")
# source("./scripts/visualizations.R")

# countries in shapefile
countries <- sort(unique(east_africa_shp$Country))

# variables for selection. Can add or reduce
variables <- c("Population", "dog_population", "rabid_dogs", "total_rabid_bites", 
               "total_people_PEP", "rabies_deaths", "lives_saved", "cost_per_life_saved")


east_africa_shp2<- output_det_model

# ui #####

ui <- fluidPage(
  
  titlePanel(title=div(img(src="./rabies_virus.png", height = 70), "A framework to inform rabies policy in East Africa"),
            windowTitle = "A framework to inform rabies policy in East Africa"),

  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId="Country", label="Select a country:", choices = countries),
      sliderInput(inputId="vax_cov", label= "Dog vaccination coverage:", min=0, max=1, value=0, step = 0.1),
      selectInput(inputId="variable", label="Select a variable:", choices = variables),
      selectInput(inputId="PEP_policy", label="Policy choice for PEP:", choices = c("Offered under status quo", "Offered free of charge")),
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
        
        #tabPanel("About", verbatimTextOutput("summary")), 
        
        tabPanel("About", includeHTML("about.html"))
        
      )
    )
  )
)



# server #####

server <- function(input, output) {
  
  # filter out selected country
  selected_country<- reactive({
    
    selected_country <- east_africa_shp2 %>%
      dplyr::filter(Country == input$Country,
                    vax_cov == input$vax_cov,
                    policy_choice == input$PEP_policy)
    
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
    return(selected_country)
  })
  
  
  
  
  # map panel 
  output$mymap <- renderLeaflet({
    
    # color palette
    pal <-
      colorBin(
        palette = "YlOrRd",
        domain = selected_country()[[input$variable]])
    
    # pop up message
    labels <- 
      sprintf(
        "<strong>%s</strong><br/>%s",
        selected_country()$County, scales::comma(selected_country()[[input$variable]])) %>% 
      lapply(htmltools::HTML)
    
    # passing the shp df to leaflet
    leaflet(selected_country()) %>%
      
      # adding tiles, without labels to minimize clutter
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      
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
                values = selected_country()[[input$variable]],
                position = "bottomright",
                title = input$variable,
                opacity = 0.8,
                na.label = "No data")
  })
  
  
  # visualize data 
  
  output$plot <- renderPlot({
    
    if(input$Country == "Kenya"){
      KE_plot
    } else if (input$Country == "Uganda"){
      UG_plot
    }else{
      TZ_plot
    }
    

    #transform the character name into a symbol
    #  select_country <- sym(input$Country)
    # selected_country() %>%
    #   #dplyr::arrange(!! select_col)%>%
    #   # use symbol unquoting with double exclamation mark !!
    #   ggplot(., aes(x=reorder(County, !! select_col), y= !! select_col))+
    #   geom_bar(stat="identity", fill="steelblue")+
    #   coord_flip() +
    #   theme_bw() +
    #   ggtitle(paste0("Total ", input$variable, " by county/district"))
    
    
  })
  
  
  
  
  # data panel
  output$table <- DT::renderDataTable({
    
    
    
    DT::datatable(selected_country() %>% st_drop_geometry() %>%
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



