COC_ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Consumer's Optimal Choice"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter the utility function. Variables: x1, x2. Preferences: a, b."),
      div(style = "margin-bottom: 20px;", tags$details(
        tags$summary("Show additional tips", class = "tagsSummary"),
        tags$ul(
          tags$li("Leontief function is not supported in this section."),
          tags$li("For the natural logarithm function ", tags$code("ln(...)"), " use ", tags$code("log(...)")),
          tags$li("For the square root function ", tags$code("sqrt(...)"), " use ", tags$code("sqrt(...)")),
          tags$li("For Euler's number ", tags$code("exp(...)"), " use ", tags$code("exp(...)")),
          tags$li(tags$b("Formulas can be enlarged"), " by right-clicking on them, going to Math Settings, Zoom Trigger, and choosing Hover, Click, or Double-Click. You only need to set this for one formula and it will apply to all others."),
          tags$li("Individual ", tags$b("curves and lines"), " on the graph ", tags$b("can be hidden"), " (and shown again) by simply clicking on them in the legend.")
        ),
      )),
      textInput("U_str_opt", "Utility Function U(x1,x2)", placeholder = "e.g., log(x1 + x2 + x1*x2)"),
      
      fluidRow(
        column(6, numericInput("a_pref_opt", "Preference a", value = 0.5, min = 0, step = 0.1)),
        column(6, numericInput("b_pref_opt", "Preference b", value = 0.5, min = 0, step = 0.1))
      ),
      
      # Budget parameters:
      fluidRow(
        column(4, numericInput("p1", "Price p1", value = 5, min = 0, step = 1)),
        column(4, numericInput("p2", "Price p2", value = 5, min = 0, step = 1)),
        column(4, numericInput("M", "Budget M", value = 50, min = 0, step = 1)),
      ),
      
      # Graph parameters:
      numericInput("x1start_opt", "Lower bound of x1", value = 0, min = 0, step = 1),
      numericInput("x1maxGraph_opt", "Upper bound of x1", value = 50, min = 0, step = 1),
      numericInput("x1step_opt", "Step size of x1", value = 0.1, min = 0, step = 0.1),
      
      actionButton("okay_opt", "Calculate"),
      actionButton("Engel", "Engel Curves"),
      actionButton("Gossen", "2nd Gossen's Law")
    ),
    
    mainPanel(
      div(style = "margin: 50px;", id = "graphCOC",
        plotlyOutput("optimGraph", width = "800px", height = "550px")
      ),
      
      h2("Formulas & Values", id = "formulasTITLE"),
      div(style = "padding: 30px;",
        fluidRow(
          column(6, uiOutput("equations_COC"), id = "equations3"),
          column(6, verbatimTextOutput("optimal_values"))
        )
      ),
      
      div(style = "margin: 50px;", id = "engel",
          plotlyOutput("EngelGraph", width = "800px", height = "550px")
      )
    )
  )
)