library(caracas)
library(reticulate)
library(plotly)


# we define the necessary symbols:
K = caracas::symbol("K")           # capital
L = caracas::symbol("L")           # labor
a = caracas::symbol("a")           # production elasticity a
b = caracas::symbol("b")           # production elasticity b




# FIRM THEORY ------------------------------------------------------------


# Calculation of the coordinates of the "tails" of the Leontief function:
leontief_Q <- function(param_vals, Q0, n, f) {
  # param_vals: (list) production elasticities a,b
  # Q0: (num) production level Q
  # n: (num) width of the individual "tails"
  # f: (num) step size for the values of L and K (for example, 0.1)
  
  # we calculate the coordinates of the right angle (the point where the two "tails" meet):
  rightAngle <- c(Q0 / param_vals$a, Q0 / param_vals$b)
  
  # horizontal line:
  h_L <- c(rightAngle[1], seq(ceiling(rightAngle[1] * 10)/10, n, by = f))   # up to the n value
  h_K <- rep(rightAngle[2], length(h_L))
  h <- data.frame(
    L = round(h_L, 1),     # we round it to one decimal place
    K = round(h_K, 1)
  )
  
  # vertical line:
  v_K <- c(rightAngle[2], seq(ceiling(rightAngle[2] * 10)/10, n, by = f))
  v_L <- rep(rightAngle[1], length(v_K))
  v <- data.frame(
    L = round(v_L, 1),
    K = round(v_K, 1)
  )
  
  table <- list(h, v)
  
  return(table)
}


# helper function for calculation of marginal productivities of Leontief function:
leontief_MP <- function(vars) {
  # vars: (list) list of L,a,b values (for example list(L = 4, a = 3, b = 4))
  
  Q <- MPL <- MPK <- MRTS <- NULL 
  
  # if we're not located at the right angle, we can easily calculate the values of Q, MPL, MPK and MRTS:
  if(length(vars$K) == 1) {

    # if a*L < b*K, Q = a*L
    if((vars$a * vars$L) < (vars$b * vars$K)) {
      Q <- paste0("a*L = ", vars$a, "*", vars$L, " = ", vars$a * vars$L)
      MPL <- vars$a
      MPK <- 0
      MRTS <- Inf
    } else if((vars$a * vars$L) > (vars$b * vars$K)) {
      Q <- paste0("b*K = ", vars$b, "*", vars$K, " = ", vars$b * vars$K)
      MPL <- 0
      MPK <- vars$b
      MRTS <- 0
    } else {
      Q <- MPL <- MPK <- MRTS <- NA
    }
    
    table <- data.frame(
      K = vars$K,
      Q = Q,
      MPL = MPL,
      MPK = MPK,
      MRTS = MRTS
    )
    
    # right angle:
  } else {
    coordinate_K <- (vars$a * vars$L) / vars$b
    coordinate_K <- round(coordinate_K, 1)
    
    table <- data.frame(
      K = c(paste0("area K > (a*L)/b => K > ", coordinate_K), paste0(coordinate_K)),
      Q = c(paste0("a * L = ", vars$a, " * ", vars$L, " = ", vars$a * vars$L), vars$a * vars$L),
      MPL = c(vars$a, "undefined"),
      MPK = c(0, "undefined"),
      MRTS = c(paste0(vars$a, " / ", 0, " = ∞"), "undefined")
    )
  }
  
  names(table) <- c("capital K", "production Q", "MPL", "MPK", "MRTS")
  
  return(table)
}


# Isoquant Map:
IsoMapGraph <- function(modal_env, q_sym, Q_levels, param_vals, L_vals,
                               width = 800, height = 550,
                               method = c("analytical", "numerical")) {
  
  method <- match.arg(method)
  customDataGraph <- customDataTable <- list()
  
  
  # we create a 'list' to store the individual curves:
  for(j in seq_along(Q_levels)) {
    Q <- Q_levels[j]
    
    if(is.na(Q)) next         # we skip the levels of Q that are NA
    
    if(method == "analytical") {
      equation <- caracas::solve_sys(q_sym, Q, K)
      # equation for calculation of K in terms of L:
      solExpr <- solution_manager(equation, as.character(K), param_vals)
      # if there are multiple solutions:
      if(identical(solExpr, "wait")) {
        solExpr <- equation[[modal_env$index()]]$K
      } else if(is.null(solExpr)) {     # if no solution was found:
        return(NULL)            # we've already informed the user, therefore only NULL is returned
      }
      
      K_vals_func <- caracas::as_func(N(subs(solExpr, param_vals)))
      K_vals <- as.numeric(K_vals_func(L_vals))
      
    } else if(method == "numerical") {
      K_vals <- numerical_solution(q_sym, L_vals, param_vals, Q, L, K)
    }
    
    # we create 2 datasets -> one for the graph and one for the table:
    datasetForTable <- datasetForGraph <- data.frame(L = L_vals, K = K_vals)
    # we check and clean them:
    datasetForGraph <- checkCurve(datasetForGraph, Q, q_sym, param_vals, "f")
    datasetForTable <- checkCurve(datasetForTable, Q, q_sym, param_vals, "f", removeNegative = FALSE)
    
    # the individual isoquants are stored in the 'list' as datasets with an additional column for the level of Q:
    customDataGraph[[j]] <- cbind(datasetForGraph, Q_level = paste0("Q = ", Q))
    customDataTable[[j]] <- cbind(datasetForTable, Q_level = paste0("Q = ", Q))
  }
  
  # we combine all curves:
  plot_data <- bind_rows(customDataGraph)
  
  # we draw the graph:
  p <- plot_ly(plot_data, x = ~L, y = ~K, color = ~Q_level, 
               type = 'scatter', mode = 'lines', 
               width = width, height = height) %>%
    layout(
      title = paste("Isoquant Map for Q = ", as.character(q_sym)),
      xaxis = list(title = "labor L"),
      yaxis = list(title = "capital K"),
      legend = list(title = list(text = "Level of Production Q"))
    )
  
  results <- list(graph = p, data = bind_rows(customDataTable),
                   number_of_curves = length(customDataGraph))
  
  return(results)
}


# isoquant map for Leontief function
IsoMapGraph_LF <- function(param_vals, Q_levels, n, f) {
  # n: (num) max value of L
  # f: (num) step size for the values
  
  Q_levels <- as.numeric(Q_levels)
  Q_levels <- Q_levels[is.finite(Q_levels)]  # we remove NA and Inf values from the levels of Q
  
  # we create a 'list' to store the individual curves:
  all_curves <- lapply(Q_levels, function(Q) {
    values <- leontief_Q(param_vals, Q, n, f)
    
    # we create a dataset for the graph and we add a column for the level of Q:
    curve <- rbind(values[[1]], values[[2]])
    curve$Q_level <- paste0("Q = ", Q)
    
    return(curve)
  })
  
  # we combine all curves into one dataset for the graph:
  plot_data <- do.call(rbind, all_curves)
  
  # we draw the graph:
  p <- plot_ly(plot_data, x = ~L, y = ~K, color = ~Q_level,
               type = 'scatter', mode = 'lines') %>%
    layout(
      title = "Isoquant Map for Leontief Production Function",
      xaxis = list(title = "labor L"),
      yaxis = list(title = "capital K"),
      legend = list(title = list(text = "Level of Production Q"))
    )
  
  results <- list(graph = p, data = plot_data,
                   number_of_curves = length(Q_levels), values_Q = Q_levels)
  
  return(results)
}


# Maximum production
maxQ_calculation <- function(TC_value, r_value, w_value, q_sym, param_vals) {
  
  # we define the necessary symbols:
  r = caracas::symbol("r")           # interest rate for capital
  w = caracas::symbol("w")           # wage for labor
  
  # we write the total cost equation:
  TC_eq <- r * K + w * L
  
  subs_list <- list(
    TC = as.character(TC_value),
    r = as.character(r_value),
    w = as.character(w_value),
    a = as.character(param_vals$a),
    b = as.character(param_vals$b)
  )
  
  # maximum capital stock:
  max_K <- solve_sys(TC_eq, TC_value, K)
  subs_list[3] <- as.character(0)
  max_K <- max_K[[1]]
  max_K <- subs(max_K$K, subs_list)
  max_K <- as.character(N(max_K))
  max_K <- as.numeric(max_K)
  
  # maximum labor stock - we set r equal 0 and compute the equation:
  max_L <- solve_sys(TC_eq, TC_value, L)
  subs_list[2] <- as.character(0)  # we set r equal 0
  subs_list[3] <- w_value  # w has the previous value
  max_L <- max_L[[1]]
  max_L <- subs(max_L$L, subs_list)
  max_L <- as.character(N(max_L))
  max_L <- as.numeric(as.character(N(as_sym(max_L))))
  
  # maximum production:
  subs_list <- list(
    K = as.character(max_K),
    L = as.character(max_L),
    a = as.character(param_vals$a),
    b = as.character(param_vals$b)
  )
  
  max_Q <- subs(q_sym, subs_list)
  max_Q <- as.numeric(as.character(N(max_Q)))
  
  return(list(
    max_K = max_K,
    max_L = max_L,
    max_Q = max_Q
  ))
}



# MONOPOLY -----------------------------------------------------------------


# Table of costs, revenues and profits at different levels of production Q:
table_costs_revenue <- function(equations, Q_values, subs_list) {
  VC_list <- FC_list <- TC_list <- ATC_list <- c() 
  MC_list <- TR_list <- MR_list <- TP_list <- c()
  
  for(i in seq_along(Q_values)) {
    # we load the new value of Q:
    subs_list$Q <- Q_values[i]
    
    # we calculate the individual values of costs, revenue, etc.:
    TR_value <- as.numeric(as.character(N(subs(equations$TR, subs_list))))
    MR_value <- as.numeric(as.character(N(subs(equations$MR, subs_list))))
    TC_value <- tryCatch(as.numeric(as.character(N(subs(equations$TC, subs_list)))), error = function(e) NULL)
    FC_value <- as.numeric(as.character(N(subs(equations$FC, subs_list))))
    VC_value <- tryCatch(as.numeric(as.character(N(subs(equations$VC, subs_list)))), error = function(e) NULL)
    ATC_value <- tryCatch(as.numeric(as.character(N(subs(equations$ATC, subs_list)))), error = function(e) NULL)
    MC_value <- as.numeric(as.character(N(subs(equations$MC, subs_list))))
    TP_value <- as.numeric(as.character(N(subs(equations$TP, subs_list))))
    
    # if the production is equal to 0, we marginal values set to NA (undefined):
    if(Q_values[i] == 0 && !is.na(MR_value)) MR_value <- NA
    if(Q_values[i] == 0 && !is.na(MC_value)) MC_value <- NA
    
    # we store the calculated values:
    TR_list <- c(TR_list, TR_value)
    MR_list <- c(MR_list, MR_value)
    TC_list <- c(TC_list, TC_value)
    FC_list <- c(FC_list, FC_value)
    VC_list <- c(VC_list, VC_value)
    ATC_list <- c(ATC_list, ATC_value)
    MC_list <- c(MC_list, MC_value)
    TP_list <- c(TP_list, TP_value)
  }
  
  # we create the table itself:
  table <- data.frame(
    Q = Q_values, VC = VC_list, FC = FC_list, TC = TC_list, ATC = ATC_list, 
    MC = MC_list, TR = TR_list, MR = MR_list, TP = TP_list
  )
  
  return(table)
}


# for calculating the values of individual curves and for their visualization:
values_of_curves <- function(equations, Q_values, subs_list) {
  # Q_values: (num) values for horizontal axis (Q)
  # subs_list: (list) values for substitution in the equations (for example, list(a = 2, b = 3))
  
  # the curves ==> ATC, AVC, AFC, MC, demand and MR
  
  # we delete Q from subs_list:
  subs_list$Q <- NULL
  
  # we substitute values into equations:
  ATC <- N(subs(equations[1], subs_list))
  AVC <- N(subs(equations[2], subs_list))
  AFC <- N(subs(equations[3], subs_list))
  MC <- N(subs(equations[4], subs_list))
  demand <- N(subs(equations[5], subs_list))
  MR <- N(subs(equations[6], subs_list))
  
  # we create functions from the equations:
  ATC_function <- caracas::as_func(ATC)     
  AVC_function <- caracas::as_func(AVC)
  AFC_function <- caracas::as_func(AFC)
  MC_function <- caracas::as_func(MC)
  demand_function <- caracas::as_func(demand)
  MR_function <- caracas::as_func(MR)
  
  # we substitute Q into the functions:
  ATC_values <- as.numeric(ATC_function(Q_values))
  ATC_values[ATC_values < 0] <- NA    # we remove negative values
  
  AVC_values <- as.numeric(AVC_function(Q_values))
  AVC_values[AVC_values < 0] <- NA
  
  AFC_values <- as.numeric(AFC_function(Q_values))
  AFC_values[AFC_values < 0] <- NA
  
  demand_values <- as.numeric(demand_function(Q_values))
  demand_values[demand_values < 0] <- NA
  
  # if MC includes variables (in other words: if it's not a constant value):
  if(length(free_symbols(MC)) != 0) {
    MC_values <- as.numeric(MC_function(Q_values))
  } else MC_values <- rep(as.numeric(as.character(MC)), length(Q_values))
  
  # if MR includes variables:
  if(length(free_symbols(MR)) != 0) {
    MR_values <- as.numeric(MR_function(Q_values))
  } else MR_values <- rep(as.numeric(as.character(MR)), length(Q_values))
  
  
  if(Q_values[1] == 0) {       # if the first value of Q is equal to 0,
    MC_values[1] = NA          # we set the first value of MC and MR to undefined (NA)
    MR_values[1] = NA
  }
  
  dataset <- data.frame(
    Q = Q_values, ATC = ATC_values, AVC = AVC_values, AFC = AFC_values,
    MC = MC_values, demand = demand_values, MR = MR_values
  )
  
  return(dataset)
}

