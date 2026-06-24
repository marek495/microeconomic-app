library(caracas)
library(reticulate)
library(plotly)

# we define the necessary symbols:
x1 = caracas::symbol("x1")         # good x1
x2 = caracas::symbol("x2")         # good x2
a = caracas::symbol("a")           # preference weight a
b = caracas::symbol("b")           # preference weight b
lam = caracas::symbol("lam")       # lambda
p1_sym = caracas::symbol("p1")     # price p1
p2_sym = caracas::symbol("p2")     # price p2
M_sym = caracas::symbol("M")       # budget M



# Calculation of values of Leontief function:
leontief_U <- function(param_vals, U0, n, f) {
  # param_vals: (list) preferences a,b
  # U0: (num) level of utility U
  # n: (num) width of individual tails
  # f: (num) step between values of x1 and x2 in the tails
  
  # we calculate the coordinates of the right angle (the point where the two tails meet):
  right_angle <- c(U0 / param_vals$a, U0 / param_vals$b)
  
  # horizontal line:
  h_x1 <- c(right_angle[1], seq(ceiling(right_angle[1] * 10)/10, n, by = f))   # up to the n value so that we have enough points in the tail, and we round up to the nearest tenth
  h_x2 <- rep(right_angle[2], length(h_x1))
  h <- data.frame(
    x1 = round(h_x1, 1),     # we round the value to the nearest tenth for better display in the table
    x2 = round(h_x2, 1)
  )
  
  # vertical line:
  v_x2 <- c(right_angle[2], seq(ceiling(right_angle[2] * 10)/10, n, by = f))
  v_x1 <- rep(right_angle[1], length(v_x2))
  v <- data.frame(
    x1 = round(v_x1, 1),
    x2 = round(v_x2, 1)
  )
  
  table <- list(h, v)
  
  return(table)
  
  # how are h_x1 and v_x2 calculated:
  # for example: if right_angle[1] = 3.33; it's 3.4 after rounding up
  # so the values will go on like this: 3.33, 3.4, and then with 0.1 step, so 3.5, 3.6, and so on
}


# helper function for calculation of marginal utilites of Leontief function:
leontief_MU <- function(vars) {
  # vars: (list) list of values of x1,a,b (for example list(x1 = 4, a = 3, b = 4))
  
  U <- MU1 <- MU2 <- MRS <- NULL            # utility U, marginal utilities MU a MRS
  
  # if we're not at the right angle, we have only one good, so we check which one it is and calculate the values accordingly:
  if(length(vars$x2) == 1) {
    
    # if a*x1 < b*x2, U = a*x1
    if((vars$a * vars$x1) < (vars$b * vars$x2)) {
      U <- paste0("a * x1 = ", vars$a, " * ", vars$x1, " = ", vars$a * vars$x1)
      MU1 <- vars$a
      MU2 <- 0
      MRS <- "∞"
    } else if((vars$a * vars$x1) > (vars$b * vars$x2)) {
      U <- paste0("b * x2 = ", vars$b, " * ", vars$x2, " = ", vars$b * vars$x2)
      MU1 <- 0
      MU2 <- vars$b
      MRS <- 0
    } else {
      U <- MU1 <- MU2 <- MRS <- NA
    }
    
    table <- data.frame(
      x2 = vars$x2,
      U = U,
      MU_x1 = MU1,
      MU_x2 = MU2,
      MRS = MRS
    )
    
    # right angle:
  } else {
    coordinate_x2 <- (vars$a * vars$x1) / vars$b
    coordinate_x2 <- round(coordinate_x2, 1)
    
    table <- data.frame(
      x2 = c(paste0("area x2 > (a*x1)/b => x2 > ", coordinate_x2), 
             paste0(coordinate_x2)),
      U = c(paste0("a * x1 = ", vars$a, " * ", vars$x1, " = ", vars$a * vars$x1), 
            vars$a * vars$x1),
      MU_x1 = c(vars$a, 
                "undefined"),
      MU_x2 = c(0, 
                "undefined"),
      MRS = c(paste0(vars$a, " / ", 0, " = ∞"), 
              "undefined")
    )
  }
  
  names(table) <- c("good x2", "utility U", "MU x1", "MU x2", "MRS")
  
  return(table)
}



# indifference map:
indiffMapGraph <- function(modal_env, u_sym, U_levels, param_vals, x1_vals,
                           width = 800, height = 550,
                           method = c("analytical", "numerical")) {
  # u_sym: (caracas_symbol) utility function
  # param_vals: (list) list of preferences for substitution into the utility function
  # x1_vals: (num) values of horizontal axis
  
  
  method <- match.arg(method)
  customDataGraph <- customDataTable <- list()
  
  
  # we calculate the values of x2 for each level of U:
  for (j in seq_along(U_levels)) {
    U <- U_levels[j]
    
    if(is.na(U)) next        # blank value of U is skipped
    
    # analytical method:
    if (method == "analytical") {
      equation <- caracas::solve_sys(u_sym, U, x2)
      solExpr <- solution_manager(equation, as.character(x2), param_vals)
      if(identical(solExpr, "wait")) {
        solExpr <- equation[[modal_env$index()]]$x2
      } else if(is.null(solExpr)) {
        return(NULL)
      }
      
      x2_vals_func <- caracas::as_func(N(subs(solExpr, param_vals)))
      x2_vals <- as.numeric(x2_vals_func(x1_vals))
      
      # numerical method:
    } else if (method == "numerical") {
      x2_vals <- numerical_solution(u_sym, x1_vals, param_vals, U, x1, x2)
    }
    
    # we create 2 datasets --> for the table (where the user can see even inappropriate solutions) and the graph (without inappropriate solutions):
    datasetForTable <- datasetForGraph <- data.frame(x1 = x1_vals, x2 = x2_vals)
    # we want to remove inappropriate combinations of x1 and x2 for the graph, and remove negative values at the same time:
    datasetForGraph <- checkCurve(datasetForGraph, U, u_sym, param_vals, "c")
    # we want to remove only the inappropriate combinations of x1 and x2 (the user should be able to see the inappropriate values):
    datasetForTable <- checkCurve(datasetForTable, U, u_sym, param_vals, "c", removeNegative = FALSE)
    
    customDataGraph[[j]] <- cbind(datasetForGraph, U_level = paste0("U = ", U))
    customDataTable[[j]] <- cbind(datasetForTable, U_level = paste0("U = ", U))
  }
  
  plot_data <- bind_rows(customDataGraph)
  
  # we create the graph:
  p <- plot_ly(plot_data, x = ~x1, y = ~x2, color = ~U_level,
               type = 'scatter', mode = 'lines',
               width = width, height = height) %>%
    layout(
      title = paste("Indifference Map for U =", as.character(u_sym)),
      xaxis = list(title = "good x1"),
      yaxis = list(title = "good x2"),
      legend = list(title = list(text = "Level of Utility U"))
    )
  
  results <- list(graph = p, data = bind_rows(customDataTable),
                   number_of_curves = length(customDataGraph))
  
  return(results)
}


# Indifference Map for Leontief function:
indiffMapGraph_LF <- function(param_vals, U_levels, n, f) {
  # n: (num) max value of x1
  # f: (num) step between values of x1 and x2 in the tails
  
  U_levels <- as.numeric(U_levels)
  U_levels <- U_levels[is.finite(U_levels)]  # remove NA and Inf values
  
  # we prepare the data for all levels of U:
  all_curves <- lapply(U_levels, function(U) {
    # we call the existing function leontief_U():
    values <- leontief_U(param_vals, U, n, f)
    
    # we combine the horizontal and vertical tails into one dataset for the graph,
    # and we add a column with the level of U:
    curve <- rbind(values[[1]], values[[2]])
    curve$U_level <- paste0("U = ", U)
    
    return(curve)
  })
  
  # we combine the datasets for all levels of U into one dataset for the graph:
  plot_data <- do.call(rbind, all_curves)
  
  # we draw the graph:
  p <- plot_ly(plot_data, x = ~x1, y = ~x2, color = ~U_level,
               type = 'scatter', mode = 'lines') %>%
    layout(
      title = "Indifference Map for Leontief Utility Function",
      xaxis = list(title = "good x1"),
      yaxis = list(title = "good x2"),
      legend = list(title = list(text = "Level of Utility U"))
    )
  
  results <- list(graph = p, data = plot_data,
                   number_of_curves = length(U_levels), values_U = U_levels)
  
  return(results)
}




# CONSUMER'S OPTIMAL CHOICE --------------------------------------

# calculation of values of Engel curves for both goods (x1* and x2*) for each level of budget M, using the optimal choice formulas (analytical solution):
engel_values_sym <- function(opt_equations, levels_M, subs_list) {
  # opt_equations: (caracas_symbol) equations x1_opt_equation & x2_opt_equation
  # levels_M: (num) budget levels M
  # subs_list: (list) list of all values (preferences, prices, budget)
  
  values_x1 <- values_x2 <- c()     # calculated values of x1* & x2*
  
  # we calculate values of x1* for each budget level:
  for(j in seq_along(levels_M)) {
    M_val <- levels_M[j]    # we select the value of M
    subs_list$M <- M_val    # we update the value of M in subs_list
    result <- as.numeric(as.character(N(subs(opt_equations[1], subs_list))))
    if(result >= 0) {
      values_x1 <- c(values_x1, result)
    } else values_x1 <- c(values_x1, NA)    # if the result is negative, we set it to NA
  }
  
  # we combine the values:
  plot_data_1 <- data.frame(M = levels_M, good = values_x1)
  
  # values for x2*
  for(j in seq_along(levels_M)) {
    M_val <- levels_M[j]    # we select the value of M
    subs_list$M <- M_val    # we update the value of M in subs_list
    result <- as.numeric(as.character(N(subs(opt_equations[2], subs_list))))
    if(result >= 0) {
      values_x2 <- c(values_x2, result)
    } else values_x2 <- c(values_x2, NA)   # if the result is negative, we set it to NA
  }
  
  # we combine the values:
  plot_data_2 <- data.frame(M = levels_M, good = values_x2)
  
  return(list(plot_data_1 = plot_data_1,
              plot_data_2 = plot_data_2))
}


# in case of complicated utility functions, we can calculate the values of Engel curves 
# for both goods (x1* and x2*) for each level of budget M using numerical optimization (numerical solution):
engel_values_num <- function(u_fun, levels_M, subs_list) {
  # u_fun: (function) utility function (using caracas::as_func(...))
  # levels_M: (num) budget levels M 
  # subs_list: (list) list of all values (preferences, prices, budget)
  
  # values x1* & x2* for each level of M:
  values_x1 <- values_x2 <- numeric(length(levels_M))
  
  # we want to keep only the parameters that are actually used in the utility function, 
  # so we check which parameters are in the utility function and we keep only those:
  params <- subs_list[!names(subs_list) %in% c("p1", "p2", "M")]
  
  for (i in seq_along(levels_M)) {
    
    M <- levels_M[i]
    p1 <- subs_list$p1
    p2 <- subs_list$p2
    
    x1_max <- M / p1   # maximum value of x1 (when x2 = 0)
    
    obj_fun <- function(x1) {
      x2 <- (M - p1 * x1) / p2
      if (x2 < 0) return(Inf)  # invalid
      
      # we find parameters of u_fun function:
      fun_args <- names(formals(u_fun))
      
      # we keep only the parameters that are actually used in the utility function:
      params <- params[names(params) %in% fun_args]
      
      # we combine the values of x1, x2 and the parameters into a list for the do.call function:
      args_list <- c(list(x1 = x1, x2 = x2), params)
      
      # Evaluate utility safely
      -do.call(u_fun, args_list)
    }
    
    # optimalization:
    opt <- optimize(obj_fun, c(0, x1_max))
    
    values_x1[i] <- opt$minimum
    values_x2[i] <- (M - p1 * opt$minimum) / p2
  }
  
  list(
    plot_data_1 = data.frame(M = levels_M, good = values_x1),
    plot_data_2 = data.frame(M = levels_M, good = values_x2)
  )
}


# we create the graph of Engel curves for both goods (x1* and x2*) using plotly:
engel_curves_graph <- function(plot_data) {
  
  p <- plot_ly() %>%
    
    # x1*
    add_trace(
      data = plot_data[1]$plot_data_1,
      x = ~M,
      y = ~good,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'orange', width = 2),
      marker = list(color = 'darkorange', size = 8),
      name = 'good x1*'
    ) %>%
    
    # x2*
    add_trace(
      data = plot_data[2]$plot_data_2,
      x = ~M,
      y = ~good,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'blue', width = 2),
      marker = list(color = "darkblue", size = 8),
      name = 'good x2*'
    ) %>%
    
    # graph layout:
    layout(
      title = "Engel Curves",
      xaxis = list(title = 'budget M'),
      yaxis = list(title = 'goods x1* and x2*')
    )
  
  
  return(p)
}
