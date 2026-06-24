firm_ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Firm Theory"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter the production function. Variables: L, K. Production Elasticities: a, b"),
      div(style = "margin-bottom: 20px;", tags$details(
        tags$summary("Show additional tips", class = "tagsSummary"),
        tags$ul(
          tags$li("Format for Leontief function is ", tags$code("min(a*L, b*K)"), " (enter exactly this form)"),
          tags$li("For mathematical function ", tags$code("ln(...)"), " use ", tags$code("log(...)")),
          tags$li("For square root use ", tags$code("sqrt(...)")),
          tags$li("For Euler's number use ", tags$code("exp(...)")),
          tags$li(tags$b("You can enlarge the formulas"), " by right-clicking on them, going to Math Settings, Zoom Trigger, and choosing Hover, Click, or Double-Click. You only need to set this for one formula and it will apply to all others."),
          tags$li("Individual ", tags$b("isocosts"), " on the graph ", tags$b("can be hidden"), " (and shown again) by simply clicking on them in the legend.")
        ),
      )),
      textInput("Q_str", "Production Function Q(L,K)", placeholder = "e.g., L^a * K^b"),
      
      fluidRow(
        column(3, numericInput("Q1", "Production Q1", value = NA, min = 0, step = 1)),
        column(3, numericInput("Q2", "Production Q2", value = NA, min = 0, step = 1)),
        column(3, numericInput("Q3", "Production Q3", value = NA, min = 0, step = 1)),
        column(3, numericInput("Q4", "Production Q4", value = NA, min = 0, step = 1)),
      ),
      
      fluidRow(
        column(6, numericInput("a_elast", "Elasticity a", value = 0.5, min = 0, step = 0.1)),
        column(6, numericInput("b_elast", "Elasticity b", value = 0.5, min = 0, step = 0.1))
      ),
      
      numericInput("L_start", "Lower bound of L", value = 0, min = 0, step = 1),
      numericInput("L_end", "Upper bound of L", value = 50, min = 0, step = 1),
      numericInput("L_step", "Step size for L", value = 0.1, min = 0, step = 0.1),
      
      actionButton("okay_firm", "Calculate")
    ),
    
    mainPanel(
      div(style = "margin: 50px;", 
          plotlyOutput("isoquantGraph", width = "800px", height = "550px")
      ),
      
      div(style = "margin: 50px;color:'black';",
          h2("Formulas", id = "formulas2"),
          uiOutput("formulas_firm")
      ),
      
      h2("Additional Calculations", id = "values2"),
      tabsetPanel(
        tabPanel("Maximum Production",
                 div(id = "panel_Q",
                   fluidRow(
                     column(6,
                            numericInput("TC", "Total Costs (TC)", value = 50, min = 0, step = 1, width = "150px"),
                            numericInput("r", "Price of Capital (r)", value = 5, min = 0, step = 1, width = "150px"),
                            numericInput("w", "Price of Labor (w)", value = 5, min = 0, step = 1, width = "150px"),
                     ),
                     column(6, verbatimTextOutput("max_Q_results"))
                   )
                 )
                ),
        tabPanel("Marginal Productivities",
                 div(style = "padding: 20px;",
                    fluidRow(
                     column(3,
                        selectInput("list_of_curves_Q", "Select a specific curve:", choices = NULL),
                        numericInput("selected_L", "Select L value:", value = 10, min = 0, step = 1, width = "150px")
                     ),
                     column(9, DT::DTOutput("MP_results"))
                    )
                 ),
                 helpText("Note: The selected value must be within the displayed graph.", id = "info_firm")
                )
              )
            )
    )
)