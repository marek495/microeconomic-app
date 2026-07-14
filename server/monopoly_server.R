monopoly_server <- function(input, output, session) {
  # Define necessary symbols/variables:
  def_sym("K", "K0", "L", "a", "b", "Q", "r", "w", "p")

  # create a new environment with some numeric values:
  values <- new.env()
  values$levels_Q <- c()
  values$Q_opt_num <- values$p_opt_num <- values$ATC_opt_num <- NULL
  values$subs_list <- list()

  # create another environment to hold formulas for revenues and costs:
  equations <- new.env()
  equations$TR <- equations$MR <- equations$FC <- equations$VC <- equations$TC <- NULL
  equations$AFC <- equations$AVC <- equations$ATC <- equations$MC <- equations$TP <- NULL
  equations$demand <- equations$q_sym <- NULL

  # hide headings:
  hide("formulas4")
  hide("title_monopoly")



  # Main button ---------------------------------------------------------

  observeEvent(input$okay_monopoly, {
    
    # always hide all headings when the button is pressed:
    hide("formulas4")
    hide("title_monopoly")
    
    
    withProgress(message = "Wait please...", value = 0, {
      incProgress(0.1, detail = "We are processing your values...")


      # Processing input values ------------------------------------------------------

      # process the production function:
      equations$q_sym <- tryCatch(
        as_sym(input$Q_monopoly),
        error = function(e) NULL
      )

      if (is.null(equations$q_sym)) {
        showModal(
          modalDialog(
            title = "Error!", "Your function could not be processed! Please check its notation.",
            tags$br(), tags$br(), tags$br(), tags$br(),
            "If you want to work with a Leontieff function, this section is not supported.",
            footer = modalButton("Close")
          )
        )
        return(NULL)
      } else {
        # if user wrote K instead of K0, we will automatically substitute it:
        equations$q_sym <- N(subs(equations$q_sym, K, K0))
      }

      # identify variables from the provided production function:
      variables <- caracas::free_symbols(equations$q_sym)
      if (check_vars(variables, c("L", "K0")) == FALSE) {
        showModal(
          modalDialog(
            title = "Error!",
            "One or more production factors (L or K0) are missing. Please check your function!",
            footer = modalButton("Close")
          )
        )
        return(NULL)
      }

      # process the demand function:
      tryCatch(
        {
          equations$demand <- as_sym(input$demand_str)
        },
        error = function(e) {
          showModal(modalDialog(
            title = "Error!", "Your demand function could not be processed!",
            footer = modalButton("Close")
          ))
        }
      )

      # identify variables from the provided demand function:
      variables <- caracas::free_symbols(equations$demand)
      if (check_vars(variables, c("Q")) == FALSE) {
        showModal(
          modalDialog(
            title = "Error!",
            "The variable Q is missing in the demand function.",
            footer = modalButton("Close")
          )
        )
        return(NULL)
      }

      # collect all Q values:
      values$levels_Q <- na.omit(c(
        input$Q1_M, input$Q2_M, input$Q3_M,
        input$Q4_M, input$Q5_M, input$Q6_M
      ))

      # assemble all input values together:
      values$subs_list$a <- input$a_elast_M
      values$subs_list$b <- input$b_elast_M
      values$subs_list$K0 <- input$K0_M
      values$subs_list$r <- input$r_M
      values$subs_list$w <- input$w_M
      values$subs_list$Q <- 0
      values$subs_list$start <- input$LB_Q
      values$subs_list$end <- input$UB_Q
      values$subs_list$freq <- input$SZ_Q



      # Compute formulas ---------------------------------------------------------

      incProgress(0.3, detail = "We are computing the equations...")

      # total revenue TR
      equations$TR <- equations$demand * Q

      # marginal revenue MR
      equations$MR <- caracas::der(equations$TR, Q)

      # fixed costs FC:
      equations$FC <- r * K0

      # variable costs VC:
      # first we need to derive labor L from the production function:
      tryCatch(
        {
          L_equation <- caracas::solve_sys(Q, equations$q_sym, L)
          L_equation <- solution_manager(L_equation, "L", values$subs_list)
          # in case of multiple possible analytical solutions:
          if (identical(L_equation, "wait")) {
            incompatibility()
          } else {   # if the solution is unique, we can proceed as usual:
            # otherwise compute as usual
            equations$VC <- w * L_equation
          }
        },
        error = function(e) equations$VC <- NULL
      )

      # total costs TC:
      if (!is.null(equations$VC)) {
        equations$TC <- equations$FC + equations$VC
      } else {
        equations$TC <- NULL
      }

      # average fixed costs AFC:
      equations$AFC <- equations$FC / Q

      # average variable costs AVC:
      if (!is.null(equations$VC)) {
        equations$AVC <- equations$VC / Q
      } else {
        equations$AVC <- NULL
      }

      # average total costs ATC:
      if (!is.null(equations$AVC)) {
        equations$ATC <- equations$AFC + equations$AVC
      } else {
        equations$ATC <- NULL
      }

      # marginal costs MC:
      if (!is.null(equations$TC)) {
        equations$MC <- caracas::der(equations$TC, Q)
      } else {
        equations$MC <- NULL
      }

      # profit TP:
      if (!is.null(equations$TC)) {
        equations$TP <- equations$TR - equations$TC
      } else {
        equations$TP <- NULL
      }



      # Table of revenues, costs and profit ---------------------------------------

      incProgress(0.5, detail = "We are creating the table...")

      show("title_monopoly")
      output$table_M <- renderDT({
        table_data <- table_costs_revenue(equations, values$levels_Q, values$subs_list)

        datatable(table_data,
          rownames = FALSE, extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            buttons = list(list(
              extend = "excel", text = "Download to Excel", filename = "monopoly_data", title = "Decomposition of Revenues and Costs of the Monopoly"
            )),
            searching = FALSE, paging = FALSE, info = FALSE
          )
        ) |>
          DT::formatRound(columns = 2:9, digits = 3) %>%
          formatStyle(
            # highlight the profit column:
            "TP",
            target = "row",
            backgroundColor = styleInterval(
              cuts = 0,
              values = c("#ffc7bf", "#bfffc0")
            )
          )
      })



      # Optimal decision of the monopoly ------------------------------------------

      incProgress(0.7, detail = "Finding the optimal decision...")
      output$optimum_M <- renderPrint({
        values$subs_list$Q <- NULL # remove Q
        # substitute all other values:
        equations$MR <- N(subs(equations$MR, values$subs_list))
        equations$MC <- N(subs(equations$MC, values$subs_list))

        # it is most efficient to find the optimum NUMERICALLY:
        f_num <- caracas::as_func(equations$MR - equations$MC)
        optimum_f <- function(Q) f_num(Q)
        lower_boundary <- lowerBoundary(optimum_f)
        values$Q_opt_num <- uniroot(optimum_f, lower = lower_boundary, upper = 1e6)$root
        cat("Optimal production level Q* =", values$Q_opt_num, "\n")

        # price for this optimal quantity:
        values$subs_list$Q <- values$Q_opt_num
        values$p_opt_num <- as.numeric(as.character(N(subs(equations$demand, values$subs_list))))
        cat("Optimal price p* =", values$p_opt_num, "\n")

        # ATC(Q*):
        values$ATC_opt_num <- as.numeric(as.character(N(subs(equations$ATC, values$subs_list))))
        cat("ATC(Q*) =", values$ATC_opt_num, "\n")
      })



      # Create plot --------------------------------------------------------

      incProgress(0.9, detail = "Creating the plot...")

      output$monopolyGraph <- renderPlotly({
        # create Q values according to the interval requested by the user:
        Q_values <- seq(values$subs_list$start, values$subs_list$end, by = values$subs_list$freq)
        # compute all the values of the functions for these Q values:
        equationsGraph <- c(equations$ATC, equations$AVC, equations$AFC, equations$MC, equations$demand, equations$MR)
        plot_data <- values_of_curves(equationsGraph, Q_values, values$subs_list)

        plot_ly(plot_data, x = ~Q) %>%
          add_lines(y = ~ATC, name = "ATC", line = list(color = "blue")) %>%
          add_lines(y = ~AVC, name = "AVC", line = list(color = "red")) %>%
          add_lines(y = ~AFC, name = "AFC", line = list(color = "green")) %>%
          add_lines(y = ~MC, name = "MC", line = list(color = "purple")) %>%
          add_lines(y = ~demand, name = "demand", line = list(color = "brown")) %>%
          add_lines(y = ~MR, name = "MR", line = list(color = "orange")) %>%
          layout(
            title = "Optimal Decision of the Monopoly",
            xaxis = list(title = "Quantity Q", range = c(min(Q_values), max(Q_values))),
            yaxis = list(title = "", range = c(0, 60)),
            shapes = list(
              list(
                type = "rect",
                x0 = 0, x1 = values$Q_opt_num,
                y0 = values$ATC_opt_num, y1 = values$p_opt_num,
                fillcolor = "rgba(0, 200, 0, 0.3)", # green, transparent
                line = list(width = 0) # no border
              )
            ),
            annotations = list(
              list(
                x = values$Q_opt_num / 2, y = (values$p_opt_num + values$ATC_opt_num) / 2,
                text = "Profit area",
                showarrow = FALSE
              )
            ),
            legend = list(title = list(text = "Curves"))
          )
      })



      # Formulas ------------------------------------------------------------------

      # now show the heading:
      show("formulas4")
      output$formulas_monopoly <- renderUI({
        # first, recompute MC and MR because earlier we substituted values into them;
        # compute their analytical form again:
        # marginal costs MC:
        if (!is.null(equations$TC)) {
          equations$MC <- caracas::der(equations$TC, Q)
        } else {
          equations$MC <- NULL
        }
        # marginal revenue MR
        equations$MR <- caracas::der(equations$TR, Q)


        # convert formulas to TeX format:
        Q_TEX <- texExport(equations$q_sym)
        demand_TEX <- texExport(equations$demand)
        TR_TEX <- texExport(equations$TR)
        MR_TEX <- texExport(equations$MR)
        FC_TEX <- texExport(equations$FC)
        VC_TEX <- texExport(equations$VC)
        TC_TEX <- texExport(equations$TC)
        AFC_TEX <- texExport(equations$AFC)
        AVC_TEX <- texExport(equations$AVC)
        ATC_TEX <- texExport(equations$ATC)
        MC_TEX <- texExport(equations$MC)
        TP_TEX <- texExport(equations$TP)

        withMathJax(HTML(paste0(
          "<b>Your production function:</b>$$Q=", Q_TEX, "$$<br>",
          "<b>Your demand function:</b>$$p(Q)=", demand_TEX, "$$<br>",
          "<b>Total revenue:</b>$$TR=p(Q)*Q=", TR_TEX, "$$<br>",
          "<b>Marginal revenue:</b>$$MR=\\frac{\\partial TR}{\\partial Q}=", MR_TEX, "$$<br>",
          "<b>Fixed costs:</b>$$FC=", FC_TEX, "$$<br>",
          "<b>Variable costs:</b>$$VC=", VC_TEX, "$$<br>",
          "<b>Total costs:</b>$$TC=FC+VC=", TC_TEX, "$$<br>",
          "<b>Average fixed costs:</b>$$AFC=\\frac{FC}{Q}=", AFC_TEX, "$$<br>",
          "<b>Average variable costs:</b>$$AVC=\\frac{VC}{Q}=", AVC_TEX, "$$<br>",
          "<b>Average total costs:</b>$$ATC=\\frac{TC}{Q}=", ATC_TEX, "$$<br>",
          "<b>Marginal costs:</b>$$MC=\\frac{\\partial TC}{\\partial Q}=", MC_TEX, "$$<br>",
          "<b>Profit:</b>$$TP=TR-TC=", TP_TEX, "$$<br>"
        )))
      })
    })
  })
}
