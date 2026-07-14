COC_server <- function(input, output, session) {
  # let's define the necessary symbols:
  x1 <- caracas::symbol("x1") # good x1
  x2 <- caracas::symbol("x2") # good x2
  a <- caracas::symbol("a") # preference a
  b <- caracas::symbol("b") # preference b
  lam <- caracas::symbol("lam") # lambda
  p1_sym <- caracas::symbol("p1") # price p1
  p2_sym <- caracas::symbol("p2") # price p2
  M_sym <- caracas::symbol("M") # budget M

  # create an environment to store variables
  # (so they are available throughout this server):
  envCOC <- new.env()
  
  # in the following calculations several errCOC may occur
  # (specifically 4, related to whether something can or cannot be
  # solved analytically), so we will record them here step by step
  # and based on that our program will decide how to handle each issue:
  envCOC$status <- list()

  # initially hide certain redundant UI elements:
  hide("equations3")
  hide("formulasTITLE")
  hide("graphCOC")
  hide("engel")
  # and disable these two buttons:
  disable("Engel")
  disable("Gossen")

  # we will use this modal window to display the analysis of the solutions for x2 (when we solve the equation analytically and get multiple solutions, or when we can't solve it analytically at all):
  showWindowServer("COCModal", COC_modal_env, "x2")
  
  # important variables for the modal window:
  COC_modal_env <- new.env()
  COC_modal_env$data <- NULL
  COC_modal_env$index <- reactiveVal(1)
  COC_modal_env$selected <- reactiveVal(NULL)
  COC_modal_env$subject <- "coc"
  

  # Main Button ---------------------------------------------------------

  observeEvent(input$okay_opt, {
    # we clean the previous results (if they exist):
    envCOC$x1_opt_equation <- envCOC$x2_opt_equation <- NULL
    envCOC$x1_opt_num <- envCOC$x2_opt_num <- envCOC$u_opt_num <- NULL
    envCOC$subs_list <- NULL
    envCOC$u_sym <- NULL
    envCOC$data_ic <- NULL
    envCOC$param_vals <- list(a = NULL, b = NULL)

    envCOC$status <- list(
      partial_deriv_error = FALSE, # partial derivative
      x2_equation_error = FALSE, # equation for x2
      x1_opt_error = FALSE, # equation for x1*
      x2_opt_error = FALSE, # equation for x2*
      analytical_method = TRUE, # default is to try analytical method
      numerical_method = FALSE # in the case that the analytical method cannot be used
    )

    COC_modal_env$data <- NULL
    COC_modal_env$index(1)
    COC_modal_env$selected(NULL)

    # we hide certain redundant UI elements:
    hide("equations3")
    hide("formulasTITLE")
    hide("graphCOC")
    hide("engel")
    hide("optimal_values")
    # and turn off these two buttons:
    disable("Engel")
    disable("Gossen")


    tryCatch(
      {
        withProgress(message = "Wait please...", value = 0, {
          incProgress(0.1, detail = "Processing your values...")

          # Processing values ------------------------------------------------------

          # process the utility function:
          envCOC$u_sym <- tryCatch(
            as_sym(input$U_str_opt),
            error = function(e) NULL
          )

          if (is.null(envCOC$u_sym)) {
            showModal(modalDialog(
              title = "Error!",
              "Your function could not be processed! Please check its notation.",
              footer = modalButton("Close")
            ))
            return(NULL)
          }

          # We identify the variables in the utility function and check if they are correct (x1 and x2):
          variables <- caracas::free_symbols(envCOC$u_sym) # we get all variables in the function
          if (check_vars(variables, c("x1", "x2")) == FALSE) {
            showModal(modalDialog(
              title = "Error!",
              "One or both variables (x1 or x2) are missing. Please check your function.",
              footer = modalButton("Close")
            ))
            return(NULL)
          }

          # preferences:
          envCOC$param_vals <- list(a = input$a_pref_opt, b = input$b_pref_opt)

          # we check if the user has entered the required values:
          if (is.na(input$p1) || is.na(input$p2) || is.na(input$M)) {
            showModal(modalDialog(
              title = "Error!",
              "One or more required values (p1, p2, or M) are missing.",
              footer = modalButton("Close")
            ))
            return(NULL)
          }

          # values for substitution:
          envCOC$subs_list <- c(
            envCOC$param_vals,
            list(p1 = input$p1, p2 = input$p2, M = input$M)
          )

          # We construct the Lagrangian:
          Lag <- envCOC$u_sym + lam * (M_sym - (p1_sym * x1) - (p2_sym * x2))

          
          # Partial Derivatives -----------------------------------------------------

          incProgress(0.2, detail = "Calculating partial derivatives...")
          tryCatch(
            {
              dL_dx1 <- caracas::der(Lag, x1)
              dL_dx2 <- caracas::der(Lag, x2)
              dL_dlam <- caracas::der(Lag, lam)
            },
            error = function(e) {
              # if there's an error, we won't be able to use the analytical method, but we can still proceed with the numerical method:
              envCOC$status$partial_deriv_error <- TRUE
              envCOC$status$analytical_method <- FALSE
              envCOC$status$numerical_method <- TRUE
              # we inform the user:
              show("optimal_values")
              output$optimal_values <- renderPrint({
                cat("Error in calculating partial derivatives! The analytical method cannot be used, but we will try to find the optimal values using a numerical method.\n")
              })
              return(NULL)
            }
          )


          # Equation for x2 -----------------------------------------------------------

          incProgress(0.3, detail = "Calculating equation for x2...")
          if (envCOC$status$analytical_method) { # if we were able to solve the previous step analytically
            tryCatch(
              {
                # without "lam*p1" or "lam*p2":
                dL_dx1 <- dL_dx1 + (lam * p1_sym)
                dL_dx2 <- dL_dx2 + (lam * p2_sym)

                ratio <- dL_dx1 / dL_dx2 # we put the first 2 partial derivatives in a fraction
                eq_ratio <- ratio - (p1_sym / p2_sym)

                # we calculate x2 for a given x1:
                x2_equation <- caracas::solve_sys(eq_ratio, 0, vars = x2)
                x2_equation <- solution_manager(x2_equation, as.character(x2), envCOC$param_vals)
                # in the case of multiple possible analytical solutions:
                if (identical(x2_equation, "wait")) {
                  incompatibility()
                  return(NULL)
                }
              },
              error = function(e) {
                envCOC$status$x2_equation_error <- TRUE
                envCOC$status$analytical_method <- FALSE
                envCOC$status$numerical_method <- TRUE
                show("optimal_values")
                output$optimal_values <- renderPrint({
                  cat("Error in calculating the equation for x2! The analytical method cannot be used, but we will try to find the optimal values using a numerical method.\n")
                })
                return(NULL)
              }
            )
          }


          # Equation for x1* ----------------------------------------------------------

          incProgress(0.4, detail = "Calculating equation for x1*...")
          if (envCOC$status$analytical_method) {
            tryCatch({
              # we put x2 in the budget constraint:
              budget_equation <- (p1_sym * x1) + (p2_sym * x2)
              budget_subs <- subs(budget_equation, x2, x2_equation)
              
              # we calculate possible analytical solutions for x1*:
              possibleSolutions <- caracas::solve_sys(M_sym, budget_subs, x1)
              # if there is only one solution, we choose it:
              if (length(possibleSolutions) == 1) {
                envCOC$x1_opt_equation <- possibleSolutions[[1]]$x1
              } else {
                # in the case of multiple solutions, we evaluate each numerically:
                possibleSolutions_data <- lapply(possibleSolutions, function(sol) {
                  x1_num <- N(subs(sol$x1, envCOC$subs_list))
                  
                  # we calculate the corresponding x2 from the budget equation:
                  x2_num <- N(subs(subs(x2_equation, x1, x1_num), envCOC$subs_list))
                  
                  # we evaluate certain conditions:
                  conditions <- is.finite(x1_num) && is.finite(x2_num) && x1_num >= 0 && x2_num >= 0
                  
                  # we calculate the utility only if the conditions are met:
                  U_val <- if (conditions) N(subs(envCOC$u_sym, list(x1 = x1_num, x2 = x2_num, a = envCOC$param_vals$a, b = envCOC$param_vals$b))) else -Inf
                  
                  list(x1 = x1_num, x2 = x2_num, U = U_val, conditions = conditions)
                })
                
                # we filter out the solutions that do not meet the conditions:
                suitableSolutions <- Filter(function(c) c$conditions, possibleSolutions_data)
                
                if (length(suitableSolutions) == 0) {
                  envCOC$x1_opt_equation <- NULL
                  show("optimal_values")
                  output$optimal_values <- renderPrint({
                    cat("No suitable solutions for x1* were found!\n")
                  })
                  return(NULL)
                }
                
                # we choose the solution with the highest utility value:
                best_index <- which.max(sapply(suitableSolutions, function(c) c$U))
                theBest <- suitableSolutions[[best_index]]
                
                # we save the optimal combination:
                envCOC$x1_opt_equation <- theBest$x1
                envCOC$x2_opt_equation <- theBest$x2
              }
              
            }, error = function(e) {
              envCOC$status$x1_opt_error <- TRUE
              envCOC$status$analytical_method <- FALSE
              envCOC$status$numerical_method <- TRUE
              show("optimal_values")
              output$optimal_values <- renderPrint({
                cat("Error in calculating the equation for x1*! The analytical method cannot be used, but we will try to find the optimal values using a numerical method.\n")
              })
              return(NULL)
            })
          }
          

          # Equation for x2* ----------------------------------------------------------

          incProgress(0.5, detail = "We calculate equation for x2*...")
          if (envCOC$status$analytical_method) {
            tryCatch(
              {
                envCOC$x2_opt_equation <- N(subs(x2_equation, x1, envCOC$x1_opt_equation))
              },
              error = function(e) {
                envCOC$status$x2_opt_error <- TRUE
                envCOC$status$analytical_method <- FALSE
                envCOC$status$numerical_method <- TRUE
                show("optimal_values")
                output$optimal_values <- renderPrint({
                  cat("Error in calculating the equation for x2*! The analytical method cannot be used, but we will try to find the optimal values using a numerical method.\n")
                })
                return(NULL)
              }
            )
          }



          # Calculation of optimal values ----------------------------------------------

          ## ANALYTICAL solution:  ---------------------------------------------------

          incProgress(0.6, detail = "Finding optimal values...")
          if (envCOC$status$analytical_method) {
            # We choose and calculate:
            x1_opt <- N(subs(envCOC$x1_opt_equation, envCOC$subs_list))
            x2_opt <- N(subs(envCOC$x2_opt_equation, envCOC$subs_list))

            # Optimal utility:
            u_opt <- subs(envCOC$u_sym, c(x1, x2), c(x1_opt, x2_opt))
            u_opt <- N(subs(u_opt, envCOC$subs_list))

            # in numerical format:
            envCOC$x1_opt_num <- as.numeric(as.character(x1_opt))
            envCOC$x2_opt_num <- as.numeric(as.character(x2_opt))
            envCOC$u_opt_num <- as.numeric(as.character(u_opt))

            if (any(c(envCOC$x1_opt_num, envCOC$x2_opt_num, envCOC$u_opt_num) < 0) ||
              any(is.na(c(envCOC$x1_opt_num, envCOC$x2_opt_num, envCOC$u_opt_num)))) {
              showNotification("Negative or missing values!", type = "warning")
            }

            show("optimal_values")
            output$optimal_values <- renderPrint({
              cat("Optimal values:\n\n")
              cat("x1* =", envCOC$x1_opt_num, "\n")
              cat("x2* =", envCOC$x2_opt_num, "\n")
              cat("U* =", envCOC$u_opt_num, "\n")
            })
          }

          ## NUMERICAL solution: ------------------------------------------------------

          if (envCOC$status$numerical_method) {
            # if there was an error in calculating the partial derivatives, that's where there's still a gap in our program
            if (envCOC$status$partial_deriv_error) {
              NULL
            }

            ### if we don't have an equation for x2 or x2* ---------------------------------------

            if (envCOC$status$x2_equation_error || envCOC$status$x2_opt_error) {
              # we create a numerical function out of our utility function:
              func_U <- caracas::as_func(envCOC$u_sym)

              # this function returns a number that the optimizer will want to minimize
              # by doing so, we will obtain the maximum utility level
              neg_U <- function(x) {
                # x: vector of variables x1, x2

                x1 <- x[1]
                x2 <- x[2]

                args <- list(x1 = x1, x2 = x2)

                # add preferences, if they exist:
                if ("a" %in% names(formals(func_U))) args$a <- envCOC$subs_list$a
                if ("b" %in% names(formals(func_U))) args$b <- envCOC$subs_list$b

                # function constrOptim(...) which we'll soon use, is a "minimizer"
                # we want to maximize our utility function, so we need to minimize the negative of it:
                -do.call(func_U, args)
              }

              # linear inequalities: x1>=0, x2>=0, p1*x1 + p2*x2 <= M
              # function constrOptim(...) requires linear inequalities in the form:
              # ui * x - ci >= 0
              ui <- rbind(
                c(1, 0), # x1 >= 0 -> 1*x1 + 0*x2 - 0 >= 0
                c(0, 1), # x2 >= 0 -> 0*x1 + 1*x2 - 0 >= 0
                c(-envCOC$subs_list$p1, -envCOC$subs_list$p2) # -p1*x1 - p2*x2 + M >= 0 <=> p1*x1 + p2*x2 <= M
              )

              # constants on the right side of the equation
              # 0, 0 = x1, x2 (both goods must be >= 0)
              ci <- c(0, 0, -envCOC$subs_list$M)

              # "initial guess":
              guess <- c(
                0.4 * envCOC$subs_list$M / envCOC$subs_list$p1,
                0.4 * envCOC$subs_list$M / envCOC$subs_list$p2
              )

              # we solve it numerically:
              solution <- constrOptim(
                theta = guess, f = neg_U, grad = NULL, ui = ui,
                ci = ci
              )

              if (solution$convergence != 0) {
                show("optimal_values")
                output$optimal_values <- renderPrint({
                  cat("Optimal quantities were not found!")
                })
                return(NULL)
              } else {
                if (any(solution$par < 0)) showNotification("Negative optimal values for x1* or x2* found!", type = "warning")

                envCOC$x1_opt_num <- solution$par[1]
                envCOC$x2_opt_num <- solution$par[2]
                envCOC$u_opt_num <- caracas::as_func(envCOC$u_sym)
                # we find function's parameters:
                parameters <- names(formals(envCOC$u_opt_num))
                # list of values for substitution:
                myList <- list(
                  a = envCOC$param_vals$a, b = envCOC$param_vals$b,
                  x1 = envCOC$x1_opt_num, x2 = envCOC$x2_opt_num)
                # we keep only the parameters we need:
                myList <- myList[names(myList) %in% parameters]
                # substitute the values into the function:
                envCOC$u_opt_num <- do.call(envCOC$u_opt_num, myList)
                
                show("optimal_values")
                output$optimal_values <- renderPrint({
                  cat("We used numerical method for computation.\n\n")
                  cat("Optimal values:\n\n")
                  cat("x1* =", envCOC$x1_opt_num, "\n")
                  cat("x2* =", envCOC$x2_opt_num, "\n")
                  cat("U* =", envCOC$u_opt_num, "\n")
                })
              }
            }


            ### if we don't have an equation for x1* ------------------------------------------------

            if (envCOC$status$x1_opt_error) {
              # if we have 'budget_subs',
              # we simply substitute our values for p1, p2, etc. into that formula:
              if (exists("budget_subs") && !is.null(budget_subs)) {
                # optimal x1*
                x1_opt <- N(subs(budget_subs, envCOC$subs_list))
                x1_opt <- caracas::solve_sys(x1_opt, input$M, x1)

                # if x1* has no solution:
                if (length(x1_opt) == 0) {
                  show("optimal_values")
                  output$optimal_values <- renderPrint({
                    cat("Optimal quantities were not found!\n")
                  })
                  return(NULL)
                } else {
                  # optimal x2*
                  x2_opt <- N(subs(budget_equation, list(
                    p1 = envCOC$subs_list$p1, p2 = envCOC$subs_list$p2,
                    x1 = x1_opt[[1]]$x1
                  )))
                  x2_opt <- caracas::solve_sys(x2_opt, envCOC$subs_list$M, x2)

                  # optimal utility U:
                  u_opt <- N(subs(envCOC$u_sym, list(
                    x1 = x1_opt[[1]]$x1, x2 = x2_opt[[1]]$x2,
                    a = envCOC$param_vals$a, b = envCOC$param_vals$b
                  )))

                  # in numeric format:
                  envCOC$x1_opt_num <- as.numeric(as.character(x1_opt[[1]]$x1))
                  envCOC$x2_opt_num <- as.numeric(as.character(x2_opt[[1]]$x2))
                  envCOC$u_opt_num <- as.numeric(as.character(u_opt))
                }
              } else {
                # We assume that we were unable to substitute the equation for x2 into the
                # budget constraint (e.g., due to the LambertW function), but we still have
                # the equation, so we simply substitute the known values into both equations:
                
                # first we convert the equation into a function:
                x2_func <- function(x1_val, sl) {
                  number <- N(subs(x2_equation, list(x1 = x1_val, p1 = sl$p1, p2 = sl$p2)))
                  as.numeric(as.character(number))
                }

                # we put this and the budget equation together:
                equation <- function(x1) {
                  x2 <- x2_func(x1, envCOC$subs_list) # x2 equation
                  envCOC$subs_list$p1 * x1 + envCOC$subs_list$p2 * x2 - envCOC$subs_list$M # budget
                }

                # we use uniroot to find the optimal values:
                tryCatch(
                  {
                    solution <- uniroot(equation, lower = 0.001, upper = envCOC$subs_list$M / envCOC$subs_list$p1)
                  },
                  error = function(e) {
                    
                    show("optimal_values")
                    output$optimal_values <- renderPrint({
                      cat("Numeric method failed to find a root in the given interval.\n")
                    })
                    
                  }
                )
                x1_opt <- solution$root
                x2_opt <- N(subs(budget_equation, list(
                  x1 = x1_opt,
                  p1 = envCOC$subs_list$p1, p2 = envCOC$subs_list$p2
                )))
                x2_opt <- caracas::solve_sys(envCOC$subs_list$M, x2_opt, x2)
                x2_opt <- solution_manager(x2_opt, as.character(x2), envCOC$param_vals)
                # in the case of multiple possible analytical solutions:
                if (identical(x2_opt, "wait")) {
                  incompatibility()
                  return(NULL)
                }

                u_opt <- N(subs(envCOC$u_sym, list(
                  x1 = x1_opt, x2 = x2_opt,
                  a = envCOC$subs_list$a, b = envCOC$subs_list$b
                )))

                envCOC$x1_opt_num <- as.numeric(as.character(x1_opt))
                envCOC$x2_opt_num <- as.numeric(as.character(x2_opt))
                envCOC$u_opt_num <- as.numeric(as.character(u_opt))
              }

              show("optimal_values")
              output$optimal_values <- renderPrint({
                cat("We used numerical method for computation.\n\n")
                cat("Optimal values:\n")
                cat("x1* =", envCOC$x1_opt_num, "\n")
                cat("x2* =", envCOC$x2_opt_num, "\n")
                cat("U* =", envCOC$u_opt_num, "\n")
              })
            }
          }



          # Preparation of values for visualization ----------------------------------------------------

          incProgress(0.8, detail = "Preparing values for graph...")
          x1_vals <- seq(input$x1start_opt, input$x1maxGraph_opt, input$x1step_opt)

          values_budget <- c() # values for the budget constraint
          values_IC <- c() # values for the indifference curve
          x2_value <- NULL # set the initial value to NULL

          # values for the budget constraint:
          for (x1_val in x1_vals) {
            x2_value <- (input$M - (input$p1 * x1_val)) / input$p2

            values_budget <- c(values_budget, x2_value)
          }

          envCOC$data_budget <- data.frame(x1 = x1_vals, x2 = values_budget)
          envCOC$data_budget$x2[envCOC$data_budget$x2 < 0] <- NA

          # values for the indifference curve:
          if (!is.na(envCOC$u_opt_num)) { # only if we computed the value of U*
            tryCatch(
              {
                equation <- caracas::solve_sys(envCOC$u_sym, envCOC$u_opt_num, x2)
                # equation for x2:
                solExpr <- solution_manager(equation, as.character(x2), envCOC$param_vals)

                # if no solution was found:
                if (is.null(solExpr)) {
                  showModal(
                    modalDialog(
                      title = "Error!",
                      "No analytical solution for x2 exists!",
                      footer = modalButton("Close")
                    )
                  )

                  hide("equations3")
                  hide("formulasTITLE")
                  hide("graphCOC")
                  hide("engel")
                  hide("optimal_values")
                  disable("Engel")    # we turn off these buttons
                  disable("Gossen") 
                  
                  return(NULL)
                  
                } else if (identical(solExpr, "wait")) {
                  COC_modal_env$data <- solution_analysis(envCOC$u_sym, equation, list(
                    a = envCOC$param_vals$a, b = envCOC$param_vals$b
                  ), x1, x2, envCOC$u_opt_num, "c")

                  COC_modal_env$index(1)
                  COC_modal_env$selected(NULL)
                  showModal(showWindowUI("COCModal"))
                  
                } else {
                  # we update the variable (so that we can move on to the grahpical visualization):
                  COC_modal_env$selected(solExpr)
                  
                  # we need to substitute only the preferences a & b, because we already generated x1:
                  expression <- N(subs(solExpr, envCOC$param_vals))

                  # we convert it to mathematical function:
                  ourFunction <- caracas::as_func(expression, x1)

                  # now we calculate the values of indifference curve themselves:
                  values_IC <- as.numeric(ourFunction(x1_vals))

                  # we put it all in a data frame:
                  envCOC$data_ic <- data.frame(x1 = x1_vals, x2 = values_IC)
                  if (all(envCOC$data_ic$x2 < 0, na.rm = TRUE)) {
                    showNotification("All values of good x2 are negative! We will remove them.", type = "warning")
                  }
                  # we check the curve's values:
                  envCOC$data_ic <- checkCurve(envCOC$data_ic, envCOC$u_opt_num, envCOC$u_sym, envCOC$subs_list, "c")
                }
              },
              error = function(e) {
                showNotification("Analytical solution not possible! Numerical solution...", type = "error")
                
                COC_modal_env$selected(TRUE)    # so we can move on
                
                interval_x1 <- seq(input$x1start_opt, input$x1maxGraph_opt, by = 1)
                roots_x2 <- numerical_solution(envCOC$u_sym, interval_x1, envCOC$param_vals, envCOC$u_opt_num, x1, x2)

                envCOC$data_ic <- data.frame(x1 = interval_x1, x2 = roots_x2)
                if (all(envCOC$data_ic$x2 < 0, na.rm = TRUE)) {
                  showNotification("All values of good x2 are negative! We will remove them.", type = "warning")
                }
                envCOC$data_ic <- checkCurve(envCOC$data_ic, envCOC$u_opt_num, envCOC$u_sym, envCOC$subs_list, "c")
              }
            )
          }

          incProgress(0.9, detail = "Creating graph...")
        })
      },
      error = function(e) print(e)
    )
  })


  # after the analytical solution for x2 has been found:
  observeEvent(COC_modal_env$selected(), {
    # graphical solution ---------------------------------------------------
    
    # we can show the graph:
    show("graphCOC")
    output$optimGraph <- renderPlotly({
      plot_ly(width = 800, height = 550) %>%
        # budget constraint:
        add_trace(
          data = envCOC$data_budget,
          x = ~x1,
          y = ~x2,
          type = "scatter",
          mode = "lines",
          line = list(color = "black", width = 2),
          name = "Budget Constraint"
        ) %>%
        # indifference curve:
        add_trace(
          data = envCOC$data_ic,
          x = ~x1,
          y = ~x2,
          type = "scatter",
          mode = "lines",
          line = list(color = "blue", width = 2),
          name = "Indifference Curve"
        ) %>%
        # horizontal dashed line:
        add_segments(
          x = 0, xend = envCOC$x1_opt_num,
          y = envCOC$x2_opt_num, yend = envCOC$x2_opt_num,
          line = list(color = "black", dash = "dash", width = 2),
          name = " "
        ) %>%
        # vertical dashed line:
        add_segments(
          x = envCOC$x1_opt_num, xend = envCOC$x1_opt_num,
          y = 0, yend = envCOC$x2_opt_num,
          line = list(color = "black", dash = "dash", width = 2),
          name = " "
        ) %>%
        # optimal point:
        add_trace(
          x = envCOC$x1_opt_num,
          y = envCOC$x2_opt_num,
          type = "scatter",
          mode = "markers",
          marker = list(color = "red", size = 10),
          name = "Optimum"
        ) %>%
        # layout:
        layout(
          title = paste("Optimal Consumer Choice for U =", isolate(as.character(envCOC$u_sym))),
          xaxis = list(title = "Good x1"),
          yaxis = list(title = "Good x2")
        )
    })

    # equations ------------------------------------------------------------------

    # we show the title:
    show("equations3")
    show("formulasTITLE")

    # we list the equations:
    output$equations_COC <- renderUI({
      # utility function:
      U_TEX <- caracas::tex(envCOC$u_sym)
      
      # x1*
      if(is.null(envCOC$x1_opt_equation)) {
        x1text <- paste0("<b>We couldn't</b> derive the formula for x1*")
      } else {
        x1_opt_TEX <- caracas::tex(envCOC$x1_opt_equation)
        x1text <- paste0("<b>Optimal x1:</b>$$x_{1}^{*}=", x1_opt_TEX, "$$")
      }
      
      # x2*
      if(is.null(envCOC$x2_opt_equation)) {
        x2text <- paste0("<br><b>We couldn't</b> derive the formula for x2*")
      } else {
        x2_opt_TEX <- caracas::tex(envCOC$x2_opt_equation)
        x2text <- paste0("<b>Optimal x2:</b>$$x_{2}^{*}=", x2_opt_TEX, "$$")
      }
      
      withMathJax(tagList(
        HTML(paste0("<b>Your utility function:</b>$$U=", U_TEX, "$$")),
        HTML(x1text),
        HTML(x2text)
      ))
    })

    # now we can enable the buttons:
    enable("Engel")
    enable("Gossen")
  })




  # Engel Curves ---------------------------------------------------------

  # Computation:
  observeEvent(input$Engel, {
    levels_M <- c() # for storing the values of M that the user will enter

    # modal window for entering the values of M:
    showModal(modalDialog(
      title = "Enter Budget Values",
      numericInput("M1", "Budget M1", value = NA, min = 0),
      numericInput("M2", "Budget M2", value = NA, min = 0),
      numericInput("M3", "Budget M3", value = NA, min = 0),
      numericInput("M4", "Budget M4", value = NA, min = 0),
      numericInput("M5", "Budget M5", value = NA, min = 0),
      footer = tagList(
        actionButton("submit_M", "Submit"),
        modalButton("Close")
      ),
      easyClose = TRUE
    ))
  })

  # if the user submits the values of M, we calculate the corresponding values of x1* and x2* and show the graph of Engel curves:
  observeEvent(input$submit_M, {
    # we close the window:
    removeModal()

    # we store the values:
    levels_M <- na.omit(c(input$M1, input$M2, input$M3, input$M4, input$M5))

    # we calculate the corresponding values of x1* and x2*:
    # in the case, if we have their equations (analytical method):
    if (!is.null(envCOC$x1_opt_equation) && !is.null(envCOC$x2_opt_equation)) {
      plot_data <- engel_values_sym(c(envCOC$x1_opt_equation, envCOC$x2_opt_equation), levels_M, envCOC$subs_list)
    
      # in the opposite case, we resort to the numerical method:
    } else {
      u_fun <- caracas::as_func(envCOC$u_sym)
      plot_data <- engel_values_num(u_fun, levels_M, envCOC$subs_list)
    }

    # we draw the graph:
    show("engel")
    output$EngelGraph <- renderPlotly({
      engel_curves_graph(plot_data)
    })
  })



  # 2nd Gossen's Law -------------------------------------------------------


  # we check the validity of the 2nd Gossen's Law:
  observeEvent(input$Gossen, {
    # we define the symbols x1 & x2 again (for safety):
    caracas::def_sym("x1", "x2")

    # only if we have the values of x1* & x2*
    if (is.null(envCOC$x1_opt_num) && is.null(envCOC$x2_opt_num)) {
      showModal(modalDialog(
        title = "Error!",
        "We couldn't find the values of x1* or x2* ! Verification of the validity of the 2nd Gossen's Law failed.",
        footer = modalButton("Close")
      ))
      return(NULL)
    } else {
      # we calculate the marginal utilities:
      mu1_sym <- der(envCOC$u_sym, x1)
      mu2_sym <- der(envCOC$u_sym, x2)

      # we create a new list for substituting values:
      subs_list_2 <- list(
        a = input$a_pref_opt, b = input$b_pref_opt,
        x1 = envCOC$x1_opt_num, x2 = envCOC$x2_opt_num
      )

      # we calculate the values:
      mu1_num <- N(subs(mu1_sym, subs_list_2))
      mu2_num <- N(subs(mu2_sym, subs_list_2))

      left_side <- as.numeric(as.character(N(mu1_num / input$p1)))
      right_side <- as.numeric(as.character(N(mu2_num / input$p2)))

      # we evaluate the validity of the law:
      if (identical(round(left_side, 1), round(right_side, 1))) {
        evaluation <- "2nd Gossen's Law is satisfied."
        sign <- "="
      } else {
        evaluation <- "2nd Gossen's Law is not satisfied."
        sign <- "≠"
      }

      # we create the content for the modal dialog:
      content_HTML <- withMathJax(HTML(paste0(
        "<b>Solving the relationship:</b><br>",
        "$$\\frac{MU_{x_{1}}}{", p1_sym, "}=\\frac{MU_{x_{2}}}{", p2_sym, "} \\implies \\frac{", caracas::tex(mu1_sym), "}{", p1_sym, "} = ",
        "\\frac{", caracas::tex(mu2_sym), "}{", p2_sym, "}$$",
        "<br><b>After substituting the values we get:</b><br>",
        "$$\\frac{", round(as.numeric(as.character(mu1_num)), 3), "}{", input$p1, "} = ",
        "\\frac{", round(as.numeric(as.character(mu2_num)), 3), "}{", input$p2, "}$$",
        "<br><b>The result is:</b><br>",
        "$$", round(left_side, 3), sign, round(right_side, 3), "$$",
        "<br><b>", evaluation, "</b><br>"
      )))

      # we show the modal dialog:
      showModal(modalDialog(
        title = "Evaluation of the validity of the 2nd Gossen's Law",
        content_HTML,
        footer = modalButton("Close"),
        easyClose = TRUE
      ))
    }
  })
}
