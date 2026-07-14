monopoly_ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Monopoly"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter the production function. Variables: L, K0. Production Elasticities: a, b"),
      
      div(style = "margin-bottom: 20px;", tags$details(
        tags$summary("Show additional tips", class = "tagsSummary"),
        tags$ul(
          tags$li("Leontief function is not supported in this section."),
          tags$li("For the mathematical function ", tags$code("ln(...)"), " use ", tags$code("log(...)")),
          tags$li("For the square root use ", tags$code("sqrt(...)")),
          tags$li("For Euler's number use ", tags$code("exp(...)")),
          tags$li(tags$b("You can enlarge the formulas"), " by right-clicking on them, going to Math Settings, Zoom Trigger, and choosing Hover, Click, or Double-Click. You only need to set this for one formula and it will apply to all others."),
          tags$li("Individual ", tags$b("curves on the graph"), tags$b(" can be hidden"), " (and shown again) by simply clicking on them in the legend.")
        ),
      )),
      
      textInput("Q_monopoly", "Production Function Q(L,K0)", placeholder = "e.g., L^a * K0^b"),
      
      fluidRow(                      # (M as in Monopoly)
        column(6, numericInput("a_elast_M", "Elasticity a", value = 0.5, min = 0, step = 0.1)),
        column(6, numericInput("b_elast_M", "Elasticity b", value = 0.5, min = 0, step = 0.1))
      ),
      
      fluidRow(          # unnecessary fields to leave empty
        column(4, numericInput("Q1_M", "Production Q1", value = NA, min = 0, step = 1)),
        column(4, numericInput("Q2_M", "Production Q2", value = NA, min = 0, step = 1)),
        column(4, numericInput("Q3_M", "Production Q3", value = NA, min = 0, step = 1))
      ),
      fluidRow( 
        column(4, numericInput("Q4_M", "Production Q4", value = NA, min = 0, step = 1)),
        column(4, numericInput("Q5_M", "Production Q5", value = NA, min = 0, step = 1)),
        column(4, numericInput("Q6_M", "Production Q6", value = NA, min = 0, step = 1))
      ),
      
      textInput("demand_str", "Demand Function p(Q)", value = "40 - 0.5*Q"),
      
      fluidRow(
        column(4, numericInput("r_M", "Interest Rate r", value = 5, min = 0, step = 1)),
        column(4, numericInput("w_M", "Wages w", value = 5, min = 0, step = 1)),
        column(4, numericInput("K0_M", "Fixed Capital K0", value = 10, min = 0, step = 1))
      ),
      
      fluidRow(
        column(4, numericInput("LB_Q", "Lower Bound Q", value = 0, min = 0, step = 1)),
        column(4, numericInput("UB_Q", "Upper Bound Q", value = 100, min = 1, step = 1)),
        column(4, numericInput("SZ_Q", "Step Size", value = 0.5, min = 0, step = 0.1))
      ),
      
      actionButton("okay_monopoly", "Calculate")
    ),
    
    mainPanel(
      div(style = "margin: 50px;width: 90%;",
          h2("Values of Revenue, Costs and Profit", id = "title_monopoly"),
          DT::DTOutput("table_M")
        ),
      
      div(style = "margin: 50px;",
          plotlyOutput("monopolyGraph")),
      
      div(style = "margin: 50px;", verbatimTextOutput("optimum_M")),
      
      div(style = "margin: 50px;",
          h2("Formulas", id = "formulas4"),
          uiOutput("formulas_monopoly")
      )
    )
  )
)