firm_server <- function(input, output, session) {
  # define necessary symbols:
  K <- caracas::symbol("K") # capital
  L <- caracas::symbol("L") # labor
  a <- caracas::symbol("a") # production elasticity a
  b <- caracas::symbol("b") # production elasticity b


  # create an environment to store variables
  # (so they are available throughout this server):
  envFirm <- new.env()
  F_modal_env <- new.env() # for modal window (F = firm)

  F_modal_env$data <- NULL # this will store the list of solutions from solution_analysis()
  F_modal_env$index <- reactiveVal(1) # reactive index
  F_modal_env$selected <- reactiveVal(NULL) # reactive user selection
  F_modal_env$subject <- "f" # f = firm

  # create the server for the modal window:
  showWindowServer("firmModal", F_modal_env, "K")

  # at the start hide irrelevant UI elements:
  hide("panel_Q")
  hide("formulas2")
  hide("values2")
  hide("list_of_curves_Q")
  hide("selected_L")
  hide("info_firm")
  
  
  # Main button ---------------------------------------------------------

  observeEvent(input$okay_firm, {
    tryCatch(
      {
        withProgress(message = "Wait please...", value = 0, {
          # update values:
          envFirm$is_leontief <- envFirm$error <- FALSE
          envFirm$map_F <- list() # F = firm
          envFirm$L_values <- envFirm$levels_Q <- c()
          envFirm$param_vals <- list(a = 0, b = 0)

          F_modal_env$data <- NULL
          F_modal_env$index(1)
          F_modal_env$selected(NULL)

          updateNumericInput(session, "selected_L", min = input$L_start, max = input$L_end, step = input$L_step)


          # Processing values ------------------------------------------------------

          incProgress(0.1, detail = "We process your values...")

          # production elasticities:
          if (!is.na(input$a_elast)) {
            envFirm$param_vals$a <- input$a_elast
          }
          if (!is.na(input$b_elast)) {
            envFirm$param_vals$b <- input$b_elast
          }

          # production levels:
          envFirm$levels_Q <- na.omit(c(input$Q1, input$Q2, input$Q3, input$Q4))
          if (length(envFirm$levels_Q) == 0) {
            showModal(
              modalDialog(
                title = "Error!",
                "Please enter at least one production value!",
                footer = modalButton("Close")
              )
            )
            return(NULL)
          }

          # if this is not a Leontief function:
          if (!input$Q_str %in% c("min(a*L,b*K)", "min(a*L, b*K)")) {
            envFirm$is_leontief <- FALSE

            envFirm$q_sym <- tryCatch(
              as_sym(input$Q_str),
              error = function(e) NULL
            )

            if (is.null(envFirm$q_sym)) {
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
            variables <- caracas::free_symbols(envFirm$q_sym) # get all symbols from q_sym
            if (check_vars(variables, c("L", "K")) == FALSE) {
              showModal(
                modalDialog(
                  title = "Error!",
                  "One or more production factors (L or K) are missing. Please check your function!",
                  footer = modalButton("Close")
                )
              )
              return(NULL)
            }

            show("formulas2")
            show("values2")
            show("panel_Q")
            show("selected_L")
            show("info_firm")
            hide("list_of_curves_Q")


            # Analytical solution of the function ---------------------------------------------

            incProgress(0.3, detail = "We are solving the equation...")
            tryCatch(
              {
                # attempt to derive a formula for K (e.g., for the first Q value):
                equation <- caracas::solve_sys(envFirm$q_sym, envFirm$levels_Q[1], K)
                # check how many solutions the equation has:
                solExpr <- solution_manager(equation, as.character(K), envFirm$param_vals)
                # if it has only one solution:
                if (!identical(solExpr, "wait")) {
                  envFirm$error <- FALSE
                  F_modal_env$selected(solExpr)
                } else if (is.null(solExpr)) { # if no solution was found:
                  envFirm$error <- TRUE
                  showModal(
                    modalDialog(
                      title = "Error!",
                      "No analytical solution for capital K exists!",
                      footer = modalButton("Close")
                    )
                  )
                  return(NULL)
                } else {
                  envFirm$error <- FALSE
                  # if multiple solutions exist:

                  F_modal_env$data <- solution_analysis(envFirm$q_sym, equation, list(
                    a = envFirm$param_vals$a,
                    b = envFirm$param_vals$b
                  ), L, K, envFirm$levels_Q[1], "f")

                  F_modal_env$index(1)
                  F_modal_env$selected(NULL)
                  showModal(showWindowUI("firmModal"))
                }

                envFirm$error <- FALSE
              },
              error = function(e) {
                # analytical method failed, so a numerical method will follow:
                showNotification("Analytical method failed. We will use the numerical method.", type = "warning")
                envFirm$error <- TRUE
                F_modal_env$selected(TRUE)
              }
            )

            # generate L values:
            envFirm$L_values <- seq(input$L_start, input$L_end, input$L_step)
          } else {
            envFirm$is_leontief <- TRUE
            F_modal_env$selected(TRUE) # change the value so the following block (observeEvent(F_modal_env$selected(), {....) can run

            hide("formulas2")
            hide("panel_Q")
            show("values2")
            show("selected_L")
            show("info_firm")
            show("list_of_curves_Q")
          }
        })
      },
      error = function(e) print(e)
    )
  })



  # After selecting the analytical solution for K: ----------------------------------

  observeEvent(F_modal_env$selected(), {
    ## Graphical representation ----------------------------------------------------

    withProgress(message = "Wait please...", value = 0.3, {
      incProgress(0.5, detail = "Creating graph...")

      output$isoquantGraph <- renderPlotly({
        # if it is NOT Leontief:
        if (!envFirm$is_leontief) {
          # analytical method:
          if (!envFirm$error) {
            envFirm$map_F <- IsoMapGraph(F_modal_env, envFirm$q_sym, envFirm$levels_Q, envFirm$param_vals, envFirm$L_values, method = "analytical")
          } else { # if it's numerical:
            envFirm$map_F <- IsoMapGraph(F_modal_env, envFirm$q_sym, envFirm$levels_Q, envFirm$param_vals, envFirm$L_values, method = "numerical")
          }
        } else { # if it is Leontief:
          envFirm$map_F <- IsoMapGraph_LF(envFirm$param_vals, envFirm$levels_Q, input$L_end, input$L_step)
        }
        envFirm$map_F$graf
      })



      # List of formulas ----------------------------------------------------------

      output$formulas_firm <- renderUI({
        # only if this is not a Leontief function:
        if (!envFirm$is_leontief) {
          # marginal productivities:
          envFirm$MPK <- caracas::der(envFirm$q_sym, K) # derivative w.r.t. K
          envFirm$MPL <- caracas::der(envFirm$q_sym, L) # derivative w.r.t. L
          # marginal rate of technical substitution:
          envFirm$MRTS <- -(envFirm$MPL / envFirm$MPK)

          # create LaTeX versions of the formulas:
          MPK_TEX <- caracas::tex(envFirm$MPK)
          MPL_TEX <- caracas::tex(envFirm$MPL)
          MRTS_TEX <- caracas::tex(envFirm$MRTS)
          Q_TEX <- caracas::tex(isolate(envFirm$q_sym))

          # display the formulas using MathJax:
          withMathJax(
            tagList(
              HTML(paste0("<b>Your Production Function:</b> $$Q=", Q_TEX, "$$")),
              HTML(paste0("<b>Marginal Productivity of Labor L:</b> $$MPL=", MPL_TEX, "$$")),
              HTML(paste0("<b>Marginal Productivity of Capital K:</b> $$MPK=", MPK_TEX, "$$")),
              HTML(paste0("<b>Marginal Rate of Technical Substitution:</b> $$MRTS=", MRTS_TEX, "$$"))
            )
          )
        }
      })



      # Maximum production -----------------------------------------------------

      incProgress(0.7, detail = "We are calculating the maximum production...")
      output$max_Q_results <- renderPrint({
        # not for the Leontief function:
        if (!envFirm$is_leontief) {
          results_maxQ <- maxQ_calculation(input$TC, input$r, input$w, envFirm$q_sym, envFirm$param_vals)
          cat("Maximum capital stock (K) = ", round(results_maxQ[["max_K"]], 3), "\n")
          cat("Maximum labor stock (L) = ", round(results_maxQ[["max_L"]], 3), "\n")
          cat("Maximum production (Q) = ", round(results_maxQ[["max_Q"]], 3), "\n")
        }
      })



      # Marginal productivities ---------------------------------------------------

      incProgress(0.9, detail = "We are calculating the marginal productivities...")

      output$MP_results <- renderDT({
        if (!envFirm$is_leontief) {
          # prepare values from the isoquant map:
          isoquant <- envFirm$map_F$data

          chosen_K <- data.frame(
            Q = character(),
            L = numeric(), K = numeric()
          )
          # this column contains the production levels chosen by the user:
          isoquant$Q_level <- as.factor(isoquant$Q_level)

          # find corresponding values of capital K:
          for (i in levels(isoquant$Q_level)) {
            # select the corresponding part of the dataset:
            dataset <- isoquant[isoquant$Q_level == i, ]

            if (as.character(input$selected_L) %in% as.character(dataset$L)) {
              j <- dataset$K[as.character(dataset$L) == as.character(input$selected_L)]
            } else {
              j <- NA
            }

            chosen_K <- rbind(chosen_K, data.frame(
              Q = i, L = input$selected_L, K = j
            ))
          }

          # create a dataframe:
          values_MP <- data.frame(
            chosen_K,
            MPK = rep(NA, length(chosen_K$Q)),
            MPL = rep(NA, length(chosen_K$Q)),
            MRTS = rep(NA, length(chosen_K$Q))
          )

          # substitution of values:
          subs_list <- list(
            a = envFirm$param_vals$a, b = envFirm$param_vals$b,
            L = input$selected_L, K = numeric()
          )

          j <- 1
          for (i in values_MP$K) {
            subs_list$K <- i
            # calculation of values:
            if (!is.na(i) & (i >= 0)) {
              # if those formulas contain variables, substitute our values into them,
              # otherwise simply use the given value:
              MPK_val <- value_subs(envFirm$MPK, subs_list)
              MPL_val <- value_subs(envFirm$MPL, subs_list)
              MRTS_val <- value_subs(envFirm$MRTS, subs_list)
            } else {
              MPK_val <- MPL_val <- MRTS_val <- NA
            }

            # put them into the table:
            values_MP$MPL[j] <- MPL_val
            values_MP$MPK[j] <- MPK_val
            values_MP$MRTS[j] <- MRTS_val

            j <- j + 1
          }

          names(values_MP) <- c("Production Q", "Labor L", "Capital K", "MPK", "MPL", "MRTS")

          datatable(values_MP,
            rownames = FALSE, extensions = "Buttons",
            options = list(
              dom = "Bfrtip",
              buttons = list(
                list(extend = "excel", text = "Export to Excel", filename = "firm_data", title = "Firm Theory - marginal productivities and MRTS")
              ),
              searching = FALSE, paging = FALSE, info = FALSE
            )
          ) |>
            DT::formatRound(columns = c(3, 4, 5, 6), digits = 3)
          } else { # if this IS the Leontief function...
          # data of individual curves:
          dataset <- envFirm$map_F$data
          levels_of_production <- levels(as.factor(dataset$Q_level)) # e.g. "Q = 10", "Q = 20", ...
          # get the selected production level from the dropdown:
          chosen_Q <- input$list_of_curves_Q
          # if that value no longer exists in our environment
          # (for example we changed production levels), pick the first available value:
          if (!chosen_Q %in% levels_of_production) {
            chosen_Q <- if (length(levels_of_production) > 0) levels_of_production[1] else NULL
          }
          # populate the dropdown with production values:
          updateSelectInput(session, "list_of_curves_Q", choices = levels_of_production, selected = chosen_Q)
          # select values from the dataset for the given production level:
          values_L <- dataset$L[dataset$Q_level == chosen_Q]
          values_K <- dataset$K[dataset$Q_level == chosen_Q]
          # if that value exists in our data frame:
          if (!is.na(input$selected_L) && any(round(values_L, 4) == round(input$selected_L, 4))) {
            # find the corresponding K value:
            chosen_K <- values_K[round(values_L, 4) == round(input$selected_L, 4)]
            values_MP <- leontief_MP(list(L = input$selected_L, K = chosen_K, a = input$a_elast, b = input$b_elast))
            datatable(values_MP,
              rownames = FALSE, extensions = "Buttons",
              options = list(
                dom = "Bfrtip",
                buttons = list(
                  list(extend = "excel", text = "Export to Excel", filename = "firm_data", title = "Firm Theory - marginal productivities and MRTS")
                ),
                searching = FALSE, paging = FALSE, info = FALSE
              )
            )
          } else {
            values_MP <- data.frame(message = "Selected L value is not valid. Please enter a different value.")
            names(values_MP) <- c(" ")
            return(DT::datatable(values_MP, rownames = FALSE, options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE)))
          }
        }
      })
    })
  })
}
