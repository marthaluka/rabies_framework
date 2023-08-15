

ui <- fluidPage(
  
  titlePanel(title=div(img(src="./rabies_virus.png", height = 70), "A framework to inform rabies policy in East Africa"),
             windowTitle = "A framework to inform rabies policy in East Africa"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("dynamic_sidebar")
    ),
    mainPanel(
      tabsetPanel(
        id = "tab_selected",
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
        tabPanel("Simple calculator",
                 uiOutput("dynamic_main")),
        tabPanel("About", includeHTML("about.html"))
        
      )
    )
  )
)
