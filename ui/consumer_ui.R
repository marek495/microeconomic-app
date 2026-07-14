consumer_ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Consumer Theory"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter the utility function. Variables: x1, x2. Preferences: a, b"),
      div(style = "margin-bottom: 20px;", tags$details(
        tags$summary("Show additional tips", class = "tagsSummary"),
        tags$ul(
          tags$li("Format for Leontief function is ", tags$code("min(a*x1, b*x2)"), " (enter exactly this form)"),
          tags$li("For mathematical function ", tags$code("ln(...)"), " use ", tags$code("log(...)")),
          tags$li("For square root use ", tags$code("sqrt(...)")),
          tags$li("For Euler's number use ", tags$code("exp(...)")),
          tags$li(tags$b("You can enlarge the formulas"), " by right-clicking on them, going to Math Settings, Zoom Trigger, and choosing Hover, Click, or Double-Click. You only need to set this for one formula and it will apply to all others."),
          tags$li("Individual ", tags$b("indifference curves"), " on the graph ", tags$b("can be hidden"), " (and shown again) by simply clicking on them in the legend.")
        ),
      )), 
      textInput("U_str", "Utility Function U(x1,x2)", placeholder = "e.g., x1^a * x2^b"),
      
      fluidRow(
        column(3, numericInput("U1", "Utility U1", value = NA, min = 0, step = 1)),
        column(3, numericInput("U2", "Utility U2", value = NA, min = 0, step = 1)),
        column(3, numericInput("U3", "Utility U3", value = NA, min = 0, step = 1)),
        column(3, numericInput("U4", "Utility U4", value = NA, min = 0, step = 1)),
      ),
      
      fluidRow(
        column(6, numericInput("a_pref", "Preference a", value = 0.5, min = 0, step = 0.1)),
        column(6, numericInput("b_pref", "Preference b", value = 0.5, min = 0, step = 0.1))
      ),
      
      numericInput("x1start", "Lower bound of x1", value = 0, min = 0, step = 1),
      numericInput("x1maxGraph", "Upper bound of x1", value = 50, min = 0, step = 1),
      numericInput("x1step", "Step size for x1", value = 0.1, min = 0, step = 0.1),
      
      actionButton("okay_consumer", "Calculate")
    ),
    
    mainPanel(
      div(style = "margin: 50px;",
          plotlyOutput("indiffPlot", width = "800px", height = "550px")
      ),
      
      div(style = "margin: 50px;color:'black';",
          h2("Formulas", id = "formulas1"),
          uiOutput("formulas_consumer")
      ),
      
      h2("Marginal Utilities and MRS", id = "values1"),
      fluidRow(
        column(6, selectInput("list_of_curves", "Select a specific curve:", choices = NULL)),
        column(6, numericInput("chosen_x1", "Select x1 value:", value = 1, min = 0, step = 0.1, width = "150px"))    # 0.1 is just a temporary value that will be changed in the server
      ),
      helpText("Note: The selected value must be within the displayed graph.", id = "info_consumer"),
      div(style = "margin-bottom: 50px;", DT::DTOutput("mu_mrs_tab"))
    )
  )
)