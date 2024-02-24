server <- function(input, output) {
  
  output$dynamic_sidebar <- renderUI({
    if (is.null(input$tab_selected) || input$tab_selected != "Calculator") {
      return(list(
        selectInput(inputId="Country", label="Select a country:", choices = countries),
        sliderInput(inputId="horizon", label= "Horizon (years):", min=1, max=10, value=1, step = 1),
        selectInput(inputId="variable", label="Select a variable:", choices = variables),
        selectInput(inputId="scenarios", label="Select a scenario:", choices = c("Status quo", "MDV", "PEP free", "MDV and PEP free")),
        conditionalPanel(
          condition = "input.scenarios == 'PEP free' || input.scenarios == 'MDV and PEP free'",
          selectInput(inputId="PEP_admin", label="Method of PEP administration:", choices = c("Intramuscular", "Intradermal"))
        )
      ))
    } else {
      return(list(
        numericInput(inputId="pop", label="Population:", value=1E6),
        numericInput(inputId="HDR", label="Human:Dog ratio:", value=25),
        selectInput(inputId="MDV_Approach", label="MDV Approach:", choices = c("None", "Budget in USD", "Target coverage")),
        conditionalPanel(
          condition = "input.MDV_Approach == 'Budget in USD'",
          textInput(inputId="budget", label="Budget:", placeholder="Enter a positive integer")
        ),
        conditionalPanel(
          condition = "input.MDV_Approach == 'Target coverage'",
          sliderInput(inputId="target_coverage", label="Target vaccination coverage:", min=0, max=1, value=0.7, step=0.1)
        ),
        conditionalPanel(
          condition = "input.MDV_Approach != 'None'",
          sliderInput(inputId="base_vax_cov", label="Base vaccination coverage:", min=0, max=1, value=0.05, step=0.05)
        ),
        selectInput(inputId="PEP_policy_calc", label="PEP policy:", choices = c("Status quo", "Free of charge")),
        conditionalPanel(
          condition = "input.PEP_policy_calc == 'Free of charge'",
          selectInput(inputId="PEP_admin_calc", label="Method of PEP administration:", choices = c("Intramuscular", "Intradermal"))
        )
      ))
    }
  })
  
  output$dynamic_main <- renderUI({
    if (input$tab_selected == "Calculator") {
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
  
  output$table <- DT::renderDataTable({
    # Get user inputs
    variable <- input$variable
    scenario_option1 <- input$scenarios
    scenario_option2 <- input$PEP_admin
    country <- input$Country

    # Call the filter_and_transform_data function
    data_to_display <- filter_and_transform_data(variable, scenario_option1, scenario_option2, country)

    # Return 
    return(data_to_display)
  })
  
  
  
  # ... Other server-side logic goes here ...
  
}
