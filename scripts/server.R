server <- function(input, output) {
  
  output$dynamic_sidebar <- renderUI({
    if (is.null(input$tab_selected) || input$tab_selected != "Simple calculator") {
      sidebar_content <- list(
        selectInput(inputId="Country", label="Select a country:", choices = countries),
        sliderInput(inputId="horizon", label= "Horizon (years):", min=1, max=10, value=1, step = 1),
        selectInput(inputId="variable", label="Select a variable:", choices = variables),
        selectInput(inputId="scenarios", label="Select a scenario:", choices = c("Status quo", "MDV", "PEP free", "MDV and PEP free")),
        uiOutput("scenario_ui")
      )
      
      if (!is.null(input$scenarios)) {
        if (input$scenarios == "MDV") {
          return(list(
            selectInput(inputId="Approach", label="Select an approach:", choices = c("Budget in USD", "Target coverage")),
            conditionalPanel(
              condition = "input.Approach == 'Budget in USD'",
              textInput(inputId="budget", label="Budget:", placeholder="Enter a positive integer")
            ),
            conditionalPanel(
              condition = "input.Approach == 'Target coverage'",
              sliderInput(inputId="target_coverage", label= "Target vaccination coverage:", min=0, max=1, value=0.7, step = 0.1)
            )
          ))
        } else if (input$scenarios == "PEP free") {
          return(selectInput(inputId="PEP_admin", label="Method of PEP administration:", choices = c("Intramuscular", "Intradermal")))
        } else if (input$scenarios == "MDV and PEP free") {
          return(list(
            selectInput(inputId="PEP_admin", label="Method of PEP administration:", choices = c("Intramuscular", "Intradermal")),
            selectInput(inputId="Approach", label="Select an approach:", choices = c("Budget in USD", "Target coverage")),
            conditionalPanel(
              condition = "input.Approach == 'Budget in USD'",
              textInput(inputId="budget", label="Budget:", placeholder="Enter a positive integer")
            ),
            conditionalPanel(
              condition = "input.Approach == 'Target coverage'",
              sliderInput(inputId="target_coverage", label= "Target vaccination coverage:", min=0, max=1, value=0.7, step = 0.1)
            )
          ))
        } 
      }
      
      return(sidebar_content)
      
    } else {
      return(list(
        numericInput(inputId="pop", label="Population:", value=1000),
        numericInput(inputId="HDR", label="HDR:", value=0.5),
        numericInput(inputId="rabies_inc", label="Rabies Incidence:", value=10),
        sliderInput(inputId="base_vax_cov", label="Base vaccination coverage:", min=0, max=1, value=0.05, step=0.05),
        sliderInput(inputId="target_vax_cov", label="Target vaccination coverage:", min=0, max=1, value=0.7, step=0.1),
        selectInput(inputId="PEP_policy_calc", label="PEP policy:", choices = c("Free of charge", "Status quo"))
      ))
    }
  })
  
  output$dynamic_main <- renderUI({
    if(input$tab_selected == "Simple calculator") {
      return(list(
        plotOutput("calc_plot"),
        verbatimTextOutput("rabid_dogs"),
        verbatimTextOutput("exposures"),
        verbatimTextOutput("rabies_deaths")
      ))
    } else {
      return(NULL)
    }
  })
  
  # ... Other server-side logic goes here ...
  
}
