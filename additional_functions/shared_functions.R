library(shiny)
library(caracas)
library(dplyr)
library(plotly)

# required symbols:
x1 <- caracas::symbol("x1") # commodity x1
x2 <- caracas::symbol("x2") # commodity x2
K <- caracas::symbol("K") # capital
L <- caracas::symbol("L") # labor
a <- caracas::symbol("a") # production elasticity, or preference weight a
b <- caracas::symbol("b") # production elasticity, or preference weight b
lam <- caracas::symbol("lam") # lambda
p1_sym <- caracas::symbol("p1") # price p1
p2_sym <- caracas::symbol("p2") # price p2
M_sym <- caracas::symbol("M") # budget M



# This function checks whether the expression contains the given variables
check_vars <- function(vars, required) {
  all(required %in% sapply(vars, as.character))
}


# Function for substituting values into an expression
# (if the symbolic expression contains no variables and is just a value,
# simply return that value)
value_subs <- function(expr, subs_list) {
  if (length(caracas::all_vars(expr)) > 0) {
    as.numeric(as.character(N(caracas::subs(expr, subs_list))))
  } else {
    as.numeric(as.character(expr))
  }
}


# function to export a symbolic expression to LaTeX: if NULL, return "-----"
texExport <- function(v) {
  if (is.null(v)) {
    return("-----")
  } else {
    return(caracas::tex(v))
  }
}



# In case of multiple possible analytical solutions of an expression/formula,
# we need to present those solutions to the user so they can choose the correct one.
# This function computes required values and draws plots for each solution separately.
solution_analysis <- function(fn, equation, subs_list, var_indep, var_dep, constant,
                            subject = c("c", "f"), interval = 0:100) {
  # fn: (caracas_symbol) utility or production function
  # equation: list of possible symbolic solutions
  # subs_list: list of values to substitute into 'equation'
  # var_indep: (caracas_symbol or chr) independent variable, i.e. x1 or L
  # var_dep: (caracas_symbol or chr) dependent variable, i.e. x2 or K
  # interval: the interval over which we compute values (default is 0:100)

  subject <- match.arg(subject)

  output <- list()
  var_indep <- as.character(var_indep)
  var_dep <- as.character(var_dep)

  if (subject == "c") {
    subs_list$U <- constant
  } else if (subject == "f") {
    subs_list$Q <- constant
  } else {
    return(NULL)
  }

  # we need to return all formulas/values + 2 tables and 1 plot for each formula/value,
  # so the output will be a nested list:
  for (i in seq_along(equation)) {
    output[[i]] <- list(
      eq = equation[i],
      beginning = NA, end = NA, graph = NA
    )
  }

  # length(output) will equal the number of provided solutions
  # the formula of the first solution is accessible as: output[[1]]$eq[[1]][[var_dep]]
  # tables like this: output[[1]]$end, output[[1]]$beginning
  # plot like this: output[[1]]$graph


  # check each solution in turn:
  for (i in seq_along(equation)) {
    # if the solution is just a simple value, not a function:
    # (for example x2 = 5)
    if (length(free_symbols(equation[[i]][[var_dep]])) == 0) {
      next # continue to the next solution (formula is a simple value; no tables or plots needed)
    }

    # compute values of the dependent variable
    values_dep <- sapply(interval, function(val) {
      subs_list[[var_indep]] <- val
      N(subs(equation[[i]][[var_dep]], subs_list))
    })

    # Convert values to strings:
    values_dep_chr <- sapply(values_dep, as.character)

    if (all(grepl("\\*I", values_dep_chr), na.rm = TRUE) || # if all values are complex
      all(is.na(values_dep)) || # if there are no values present
      all(grepl("Inf", values_dep_chr))) { # if all values go to infinity
      output[[i]]$graph <- NA # graphical visualization failed
      next # move to the next solution
    } else { # in the opposite case....

      # create a data.frame with the computed values of both variables:
      dfForGraph <- data.frame(
        x = interval,
        y = values_dep_chr
      )
      names(dfForGraph) <- c(var_indep, var_dep)

      dfForTables <- dfForGraph

      # for tables we can format this data.frame visually, e.g. so values don't have too many decimals:
      dfForTables[[var_dep]] <- sapply(dfForTables[[var_dep]], function(val) {
        # if the value is not missing:
        if (!is.na(val)) {
          # leave complex numbers and infinity as text (string)
          if (grepl("\\*I", val) || grepl("Inf", val)) {
            val
          } else {
            # round numerical values to 3 decimal places:
            num <- as.numeric(val)
            format(round(num, 3), nsmall = 2)
          }
        }
      })

      dfForTables[[var_dep]] <- gsub("zoo", "∞", dfForTables[[var_dep]])

      if (identical(subject, "c")) {
        dfForTables <- checkCurve(dfForTables, subs_list$U, fn, list(a = subs_list$a, b = subs_list$b), subject, removeNegative = FALSE)
      } else {
        dfForTables <- checkCurve(dfForTables, subs_list$Q, fn, list(a = subs_list$a, b = subs_list$b), subject, removeNegative = FALSE)
      }

      # show the user which values are at the start and end of the curve using these tables:
      # (since complex numbers may appear that cannot be plotted, the user would otherwise
      # not know exactly what is happening)
      output[[i]]$beginning <- head(dfForTables)
      output[[i]]$end <- tail(dfForTables)

      # if complex numbers appear among the dependent variable values, replace them with NA:
      dfForGraph[[var_dep]] <- suppressWarnings(as.numeric(dfForGraph[[var_dep]]))

      # validate and clean the entire data.frame:
      if (identical(subject, "c")) {
        dfForGraph <- checkCurve(dfForGraph, subs_list$U, fn, list(a = subs_list$a, b = subs_list$b), subject)
      } else {
        dfForGraph <- checkCurve(dfForGraph, subs_list$Q, fn, list(a = subs_list$a, b = subs_list$b), subject)
      }

      # create the plot (excluding negative and complex values)
      dfForGraph[[var_dep]] <- as.numeric(dfForGraph[[var_dep]])

      curveGraph <- plot_ly(
        data = dfForGraph, x = ~ .data[[var_indep]], y = ~ .data[[var_dep]],
        type = "scatter", mode = "lines",
        line = list(color = "red", width = 1.2)
      ) %>%
        layout(
          xaxis = list(title = var_indep),
          yaxis = list(title = var_dep)
        )
      output[[i]]$graph <- curveGraph
    }
  }
  return(output)
}



# this function must be called after solving an analytical function
# it checks and manages the number of possible solutions for the function
solution_manager <- function(equation, var_dep, subs_list) {
  # equation: functions in analytical form (caracas_symbol)
  # var_dep: (chr) dependent variable, against which we define the equation ("K" or "x2")
  # subs_list: (list) list of necessary values for substitution into 'equation'

  
  # if there is only one solution, use it:
  if (length(equation) == 1) {
    solExpr <- equation[[1]][[var_dep]]
    return(solExpr)
    
  } else if (length(equation) > 1) {
    # if multiple solutions exist, display a modal window (not here) and wait for the user
    # to choose the correct solution:
    return("wait")

  } else {
    # if no solution exists, simply return nothing:
    return(NULL)
  }
}


# the "Consumer's optimal choice" and "Monopoly" parts do not support
# a modal window for selecting among multiple analytical solutions of the dependent variable
# (due to the high complexity of the application). This function notifies the user.
incompatibility <- function() {
  showModal(
    modalDialog(
      title = "Problem!",
      "We have found several possible analytical solutions for the dependent variable! We cannot process this example.",
      footer = modalButton("Close")
    )
  )
}


# Function to find the lower bound of the interval of a numerical function (created, e.g., via caracas::as_func(...)) for use with uniroot(...):
lowerBoundary <- function(func, start = 0.001, step = 0.1, max_iter = 1000) {
  lower <- start
  upper <- start + step
  iter <- 0

  while (iter <= max_iter) {
    f_lower <- func(lower)
    f_upper <- func(upper)

    if (!is.nan(f_lower) & !is.nan(f_upper) & (f_lower * f_upper < 0)) {
      return(lower)
    }

    lower <- upper
    upper <- upper + step
    iter <- iter + 1
  }

  return(NA) # if no boundary was found within the given number of iterations
}


# if a formula cannot be computed or derived analytically, use
# a numerical method with uniroot(...)
numerical_solution <- function(func, x1_vals, param_vals, constant, var_indep, var_dep) {
  # func: (caracas_symbol) function (utility/production)
  # x1_vals: values of good x1, or labor L
  # param_vals: list of preferences or elasticities a,b
  # constant: (num) target utility (U_target) or production (Q_target)
  # var_indep: (caracas_symbol) independent variable (x1 for consumer, L for firm)
  # var_dep: (caracas_symbol) dependent variable (x2 for consumer, K for firm)

  subs_f <- N(caracas::subs(func - constant, param_vals))

  n <- length(x1_vals)
  values <- numeric(n)

  for (k in seq_along(x1_vals)) {
    i <- x1_vals[k]

    subs_f2 <- N(caracas::subs(subs_f, var_indep, i))

    if (grepl("zInf", as.character(subs_f)) || grepl("zInf", as.character(subs_f2))) {
      values[k] <- NA
      next # move to the next value
    }

    # create a function from that expression:
    func <- caracas::as_func(subs_f2)

    f_r <- Vectorize(function(x) as.numeric(func(x)))

    x_seq <- seq(0.001, 1000, length.out = 200)
    y_seq <- f_r(x_seq)

    idx <- which(diff(sign(y_seq)) != 0)
    if (length(idx) > 0) {
      root <- uniroot(f_r,
        lower = x_seq[idx[1]],
        upper = x_seq[idx[1] + 1]
      )$root
    } else {
      root <- NA
    }

    values[k] <- root
  }

  return(values)
}


# used when creating an indifference curve or an isoquant
# this function does two things:
# 1. checks whether the curve values actually correspond to the chosen level of utility or production
# 2. removes negative values of x2 or K (by default; this behavior can be turned off)
checkCurve <- function(dataset, constant, equation, param_vals, subject = c("c", "f"), removeNegative = TRUE) {
  # dataset: (data.frame) list of values for the indifference curve or isoquant
  # constant: (num) level of utility U or production Q
  # equation: (caracas_symbol) utility or production function
  # param_vals: (list) list of preferences/elasticities to substitute into 'equation'
  # subject: whether this is a consumer or a firm ('c' or 'f')
  # removeNegative: (TRUE/FALSE) whether this function should also remove negative values

  subject <- match.arg(subject)

  if (subject == "c") {
    independent <- as.numeric(dataset$x1)
    dependent <- as.numeric(dataset$x2)
    name_of_dependent <- "x2"
  } else if (subject == "f") {
    independent <- as.numeric(dataset$L)
    dependent <- as.numeric(dataset$K)
    name_of_dependent <- "K"
  } else {
    return(NULL)
  }

  # substitute preferences or elasticities into the expression:
  subs_expr <- N(subs(equation, list(a = param_vals$a, b = param_vals$b)))

  # create a numeric caracas function from 'subs_expr':
  ourFunction <- caracas::as_func(subs_expr)

  # substitute values of independent and dependent variables into it
  # but first determine the exact order of arguments of that function:
  arguments <- names(formals(ourFunction))
  # if the first argument is x1 or L, substitute independent variable values first,
  # otherwise the first argument is x2 or K, so substitute dependent variable values first:
  if (arguments[1] == "x1" || arguments[1] == "L") {
    values <- as.numeric(ourFunction(independent, dependent))
  } else {
    values <- as.numeric(ourFunction(dependent, independent))
  }

  if (removeNegative == TRUE) {
    # evaluate equality to 2 decimal places, and also ensure dependent variable values are non-negative:
    suitable <- (round(values, 2) == round(constant, 2)) & (dependent >= 0)
  } else {
    # evaluate only equality to 2 decimal places; negative values will be kept:
    suitable <- (round(values, 2) == round(constant, 2))
  }

  # return the validated dataset:
  dataset[[name_of_dependent]] <- ifelse(suitable, dependent, NA)

  return(dataset)
}
