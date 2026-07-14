# for the modal window to select among multiple possible analytical solutions of the dependent variable

showWindowUI <- function(id) {
  ns <- NS(id) # creates a namespace for inputs and outputs (they will be isolated and separate for all servers)

  modalDialog(
    title = "Several analytical solutions to the dependent variable were found.",
    p("Several possible solutions for the dependent variable were found in your function. Please review them based on the following information and select the most appropriate one."),
    tags$small("Note: This application does not display complex or negative numbers. Additionally, if we need the value of utility/production, the first value U, resp. Q, that you selected is automatically chosen (empty fields are ignored). It may therefore happen that the calculated values of the dependent variable are negative or otherwise inappropriate."),
    h4(textOutput(ns("filter_solution_text"))),
    uiOutput(ns("filter_equation")),
    fluidRow(
      column(
        3, div(style = "text-align: center;", h4("Start of curve")),
        DT::DTOutput(ns("filter_Tab1"))
      ),
      column(
        3, div(style = "text-align: center;", h4("End of curve")),
        DT::DTOutput(ns("filter_Tab2"))
      ),
      column(
        6, div(style = "text-align: center;", h4("Graph")),
        plotly::plotlyOutput(ns("filter_Graph"))
      )
    ),
    footer = tagList(
      actionButton(ns("filter_next"), "Next"),
      actionButton(ns("filter_select"), "Select")
    ),
    easyClose = FALSE,
    size = "l"
  )
}



showWindowServer <- function(id, modal_env, var_dep) {
  moduleServer(id, function(input, output, session) {
    # Dynamic creation of outputs for the modal window:

    output[["filter_solution_text"]] <- renderText({
      req(modal_env$data)
      
      paste("Solution no.", modal_env$index())
    })

    output[["filter_equation"]] <- renderUI({
      req(modal_env$data)
      
      idx <- modal_env$index()
      content <- caracas::tex(modal_env$data[[idx]]$eq[[1]][[var_dep]])
      if (modal_env$subject %in% c("f", "m")) {
        withMathJax(HTML(paste0("$$K=", content, "$$")))
      } else if (modal_env$subject %in% c("c", "coc")) {
        withMathJax(HTML(paste0("$$x_2=", content, "$$")))
      }
    })

    output[["filter_Tab1"]] <- DT::renderDT({
      req(modal_env$data)
      
      tab1 <- modal_env$data[[modal_env$index()]]$beginning
      # if creating the table failed, notify the user:
      if (nrow(tab1) == 0 || all(is.na(tab1))) {
        tab1 <- data.frame(oznam = "Table is missing.")
        names(tab1) <- c(" ")
      }
      datatable(tab1,
        options = list(
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          searching = FALSE, paging = FALSE, info = FALSE
        ),
        rownames = FALSE
      )
    })

    output[["filter_Tab2"]] <- DT::renderDT({
      req(modal_env$data)

      tab2 <- modal_env$data[[modal_env$index()]]$end
      if (nrow(tab2) == 0 || all(is.na(tab2))) {
        tab2 <- data.frame(oznam = "Table is missing.")
        names(tab2) <- c(" ")
      }
      datatable(tab2,
        options = list(
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          searching = FALSE, paging = FALSE, info = FALSE
        ),
        rownames = FALSE
      )
    })

    output[["filter_Graph"]] <- plotly::renderPlotly({
      req(modal_env$data)
      
      g <- modal_env$data[[modal_env$index()]]$graph
      if (all(is.na(g))) { # display an error message
        g <- plot_ly(type = "scatter", mode = "lines") %>%
          layout(
            title = list(
              text = "Not available.",
              font = list(color = "black", size = 14)
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      }
      # return the plot itself:
      g
    })


    # 'Next' button
    observeEvent(input$filter_next, {
      modal_env$index(modal_env$index() + 1)
      if (modal_env$index() > length(modal_env$data)) {
        modal_env$index(1)
      }
    })

    # 'Select' button
    observeEvent(input$filter_select, {
      modal_env$selected(modal_env$data[[modal_env$index()]])
      removeModal()
    })
  })
}
