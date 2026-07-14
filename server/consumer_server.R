consumer_server <- function(input, output, session) {
  # define necessary symbols:
  x1 <- caracas::symbol("x1") # good x1
  x2 <- caracas::symbol("x2") # good x2
  a <- caracas::symbol("a") # preference a
  b <- caracas::symbol("b") # preference b

  # create an environment to store variables (to make them available throughout the server):
  envConsumer <- new.env()
  
  # for modal window to choose among multiple possible analytical solutions of the dependent variable (C = consumer):
  C_modal_env <- new.env()
  C_modal_env$data <- NULL # data for the modal window
  C_modal_env$index <- reactiveVal(1) # index for the modal window
  C_modal_env$selected <- reactiveVal(NULL) # selected value from the modal window
  C_modal_env$subject <- "c" # c = consumer
  # create server for this modal window:
  showWindowServer("consumerModal", C_modal_env, "x2")


  # initially hide irrelevant UI elements:
  hide("formulas1")
  hide("values1")
  hide("list_of_curves")
  hide("chosen_x1")
  hide("info_consumer")


  # Main button ---------------------------------------------------------

  observeEvent(input$okay_consumer, {
    tryCatch(
      {
        withProgress(message = "Wait please...", value = 0, {
          # reset our values:
          envConsumer$is_leontief <- envConsumer$error <- FALSE
          envConsumer$map_C <- list() # C = consumer
          envConsumer$x1_values <- envConsumer$levels_U <- c()
          envConsumer$param_vals <- list(a = 0, b = 0)
          envConsumer$MU_x1_TEX <- envConsumer$MU_x2_TEX <- envConsumer$MRS_TEX <- NULL

          C_modal_env$data <- NULL
          C_modal_env$index(1)
          C_modal_env$selected(NULL)

          # update numeric input for selecting specific x1 value according to user constraints:
          updateNumericInput(session, "chosen_x1", min = input$x1start, max = input$x1maxGraph, step = input$x1step)


          # Processing values ------------------------------------------------------

          incProgress(0.1, detail = "Processing your values...")

          # preferences:
          if (!is.na(input$a_pref)) { # if there is a value, fill it in
            envConsumer$param_vals$a <- input$a_pref
          }
          if (!is.na(input$b_pref)) {
            envConsumer$param_vals$b <- input$b_pref
          }

          # utility levels:
          envConsumer$levels_U <- na.omit(c(input$U1, input$U2, input$U3, input$U4))
          if (length(envConsumer$levels_U) == 0) {
            showModal(
              modalDialog(
                title = "Error!",
                "Please enter at least one utility value!",
                footer = modalButton("Close")
              )
            )
            return(NULL)
          }

          # if this is not a Leontief function:
          if (!input$U_str %in% c("min(a*x1,b*x2)", "min(a*x1, b*x2)")) {
            envConsumer$is_leontief <- FALSE

            envConsumer$u_sym <- tryCatch(
              as_sym(input$U_str),
              error = function(e) NULL
            )

            if (is.null(envConsumer$u_sym)) {
              showModal(
                modalDialog(
                  title = "Error!",
                  "Your function could not be processed! Please check its syntax.",
                  footer = modalButton("Close")
                )
              )
              return(NULL)
            }

            # Identify symbols from the provided function:
            variables <- caracas::free_symbols(envConsumer$u_sym) # get all symbols from u_sym
            if (check_vars(variables, c("x1", "x2")) == FALSE) {
              showModal(
                modalDialog(
                  title = "Error!",
                  "One or both variables (x1 or x2) are missing. Please check your function.",
                  footer = modalButton("Close")
                )
              )
              return(NULL)
            }

            hide("list_of_curves")
            show("formulas1")
            show("values1")
            show("chosen_x1")
            show("info_consumer")


            # Analytical solution of the function ---------------------------------------------

            incProgress(0.4, detail = "Solving the equation...")
            tryCatch(
              {
                # try to derive an expression for x2 (for example for the 1st U value):
                equation <- caracas::solve_sys(envConsumer$u_sym, envConsumer$levels_U[1], x2)
                # check how many solutions the equation has:
                solExpr <- solution_manager(equation, as.character(x2), envConsumer$param_vals)
                # if there is only one solution, simply select it:
                if (!identical(solExpr, "wait")) {
                  envConsumer$error <- FALSE
                  C_modal_env$selected(solExpr)
                  
                  # if no analytical solution exists:
                } else if (is.null(solExpr)) {
                  envConsumer$error <- TRUE
                  showModal(
                    modalDialog(
                      title = "Problem!",
                      "No analytical solution exists for good x2!",
                      footer = modalButton("Close")
                    )
                  )
                  return(NULL)
                } else {
                  envConsumer$error <- FALSE
                  # if multiple solutions exist, launch the modal window to choose the appropriate solution:

                  C_modal_env$data <- solution_analysis(envConsumer$u_sym, equation, list(
                    a = envConsumer$param_vals$a,
                    b = envConsumer$param_vals$b
                  ), x1, x2, envConsumer$levels_U[1], "c")

                  C_modal_env$index(1)
                  C_modal_env$selected(NULL)
                  showModal(showWindowUI("consumerModal"))
                }

                envConsumer$error <- FALSE
              },
              error = function(e) {
                # analytical method failed, so we will follow with the numerical method:
                showNotification("Analytical method failed. Using numerical method.", type = "warning")
                envConsumer$error <- TRUE
                # change the value so that the following block (observeEvent(C_modal_env$selected(), {....) can run
                C_modal_env$selected(TRUE)
              }
            )

            # generate x1 values:
            envConsumer$x1_values <- seq(input$x1start, input$x1maxGraph, input$x1step)
            
          } else {
            # if it's a Leontief utility function, record this in the variable:
            envConsumer$is_leontief <- TRUE
            C_modal_env$selected(TRUE)

            hide("formulas1")
            show("list_of_curves")
            show("values1")
            show("chosen_x1")
            show("info_consumer")
          }
        })
      },
      error = function(e) print(e)
    )
  })



  # After selecting an analytical solution for x2: ----------------------------------

  observeEvent(C_modal_env$selected(), {
    withProgress(message = "Wait please", value = 0.3, {
      ## Graphical representation ----------------------------------------------------

      incProgress(0.6, detail = "Creating graph...")

      output$indiffPlot <- renderPlotly({
        # if this is NOT Leontief:
        if (!envConsumer$is_leontief) {
          # analytical method
          if (!envConsumer$error) {
            envConsumer$map_C <- indiffMapGraph(C_modal_env, envConsumer$u_sym, envConsumer$levels_U, envConsumer$param_vals, envConsumer$x1_values, method = "analytical")
          } else {
            # if using numerical method:
            envConsumer$map_C <- indiffMapGraph(C_modal_env, envConsumer$u_sym, envConsumer$levels_U, envConsumer$param_vals, envConsumer$x1_values, method = "numerical")
          }
        } else { # Leontief:
          envConsumer$map_C <- indiffMapGraph_LF(envConsumer$param_vals, envConsumer$levels_U, input$x1maxGraph, input$x1step)
        }
        envConsumer$map_C$graph
      })


      # Formulas ------------------------------------------------------------------

      output$formulas_consumer <- renderUI({
        # not for the Leontief function (caracas package cannot process it symbolically):
        if (!envConsumer$is_leontief) {
          # marginal utilities:
          envConsumer$MU_x1 <- caracas::der(envConsumer$u_sym, x1) # formulas
          envConsumer$MU_x2 <- caracas::der(envConsumer$u_sym, x2)
          # marginal rate of substitution:
          envConsumer$MRS <- -(envConsumer$MU_x1 / envConsumer$MU_x2)

          # convert these formulas to LaTeX format:
          MU_x1_TEX <- caracas::tex(envConsumer$MU_x1)
          MU_x2_TEX <- caracas::tex(envConsumer$MU_x2)
          MRS_TEX <- caracas::tex(envConsumer$MRS)
          U_TEX <- caracas::tex(isolate(envConsumer$u_sym))

          # display them:
          withMathJax(tagList(
            HTML(paste0("<b>Your utility function:</b> $$U=", U_TEX, "$$")),
            HTML(paste0("<b>Marginal utility of x1:</b> $$MU_{x_{1}}=", MU_x1_TEX, "$$")),
            HTML(paste0("<b>Marginal utility of x2:</b> $$MU_{x_{2}}=", MU_x2_TEX, "$$")),
            HTML(paste0("<b>Marginal rate of substitution:</b> $$MRS=", MRS_TEX, "$$"))
          ))
        }
      })
      


      # Table of marginal utilities ----------------------------------------------

      incProgress(0.8, detail = "Creating table...")
      output$mu_mrs_tab <- renderDT({
        # if this is not a Leontief function
        if (!envConsumer$is_leontief) {

          # prepare indifference map values:
          indiffCurve <- envConsumer$map_C$data

          # prepare a data.frame for the determined x2 values:
          chosen_x2 <- data.frame(
            U = character(),
            x1 = numeric(), x2 = numeric()
          )

          # this column contains the individual utility levels selected by the user:
          indiffCurve$U_level <- as.factor(indiffCurve$U_level)

          # iterate over each curve:
          for (i in levels(indiffCurve$U_level)) {
            # select the corresponding part of the dataset:
            dataset <- indiffCurve[indiffCurve$U_level == i, ]
            # convert it to text type for reliability (we could also use rounding):
            if (as.character(input$chosen_x1) %in% as.character(dataset$x1)) {
              j <- dataset$x2[as.character(dataset$x1) == as.character(input$chosen_x1)]
            } else {
              j <- NA
            }

            chosen_x2 <- rbind(chosen_x2, data.frame(
              U = i, x1 = input$chosen_x1, x2 = j
            ))
          }

          # create data.frame (continuing with 'chosen_x2'):
          values_MU <- data.frame(
            chosen_x2,
            MU_x1 = rep(NA, length(chosen_x2$U)),
            MU_x2 = rep(NA, length(chosen_x2$U)),
            MRS = rep(NA, length(chosen_x2$U))
          )

          # list of values for substituting into formulas:
          subs_list <- list(
            a = envConsumer$param_vals$a, b = envConsumer$param_vals$b,
            x1 = input$chosen_x1, x2 = numeric()
          )

          j <- 1
          for (i in values_MU$x2) {
            subs_list$x2 <- i # selects the x2 value for the corresponding curve
            # compute values only if x2 exists for the curve and is non-negative:
            if (!is.na(i) & (i >= 0)) {
              # if those formulas contain variables, substitute our values into them,
              # otherwise simply use the given value:
              MU_x1_val <- value_subs(envConsumer$MU_x1, subs_list)
              MU_x2_val <- value_subs(envConsumer$MU_x2, subs_list)
              MRS_val <- value_subs(envConsumer$MRS, subs_list)
            } else {
              MU_x1_val <- MU_x2_val <- MRS_val <- NA
            }

            # put them into the table:
            values_MU$MU_x1[j] <- MU_x1_val
            values_MU$MU_x2[j] <- MU_x2_val
            values_MU$MRS[j] <- MRS_val

            j <- j + 1
          }
          
          names(values_MU) <- c("Utility U", "Good x1", "Good x2", "MU x1", "MU x2", "MRS")

          datatable(values_MU,
            rownames = FALSE, extensions = "Buttons", # "rownames" = removes the first column with row numbers, "extensions" adds the buttons for downloading the table
            options = list(
              dom = "Bfrtip",
              buttons = list(
                list(extend = "excel", text = "Download to Excel", filename = "consumer_data", title = "Consumer Theory - Marginal Utilities and MRS")
              ),
              searching = FALSE, paging = FALSE, info = FALSE # "info" represents (unnecessary) text at the bottom of the table
            )
          ) |>
            DT::formatRound(columns = c(3, 4, 5, 6), digits = 3)
          
          
          # if this IS a Leontief function:
        } else { 
          # data for individual curves:
          dataset <- envConsumer$map_C$data
          levels_of_utility <- levels(as.factor(dataset$U_level)) # e.g. "U = 10", "U = 20", ...
          # get the selected utility level from the dropdown:
          selection_U <- input$list_of_curves
          # if that value no longer exists in our environment
          # (for example we changed utility levels), pick the first available value:
          if (!selection_U %in% levels_of_utility) {
            selection_U <- if (length(levels_of_utility) > 0) levels_of_utility[1] else NULL
          }
          # populate the dropdown with utility values:
          updateSelectInput(session, "list_of_curves", choices = levels_of_utility, selected = selection_U)
          # select values from dataset for the given utility level:
          values_x1 <- dataset$x1[dataset$U_level == selection_U]
          values_x2 <- dataset$x2[dataset$U_level == selection_U]
          # if that value exists in our dataset:
          if (!is.na(input$chosen_x1) && any(round(values_x1, 4) == round(input$chosen_x1, 4))) {
            # find the corresponding x2 value:
            chosen_x2 <- values_x2[round(values_x1, 4) == round(input$chosen_x1, 4)]
            values_MU <- leontief_MU(list(x1 = input$chosen_x1, x2 = chosen_x2, a = input$a_pref, b = input$b_pref))
            datatable(values_MU,
              rownames = FALSE, extensions = "Buttons",
              options = list(
                dom = "Bfrtip",
                buttons = list(
                  list(extend = "excel", text = "Download to Excel", filename = "consumer_data", title = "Consumer Theory - Marginal Utilities and MRS")
                ),
                searching = FALSE, paging = FALSE, info = FALSE
              )
            )
          } else {
            values_MU <- data.frame(message = "Selected x1 value is not valid. Please enter a different value.")
            names(values_MU) <- c(" ")
            return(DT::datatable(values_MU, rownames = FALSE, options = list(dom = "t", paging = FALSE)))
          }
        }
      })
    })
  })
}
