library(dynlm)

optimal_short_run_model_function <- function(data, lag_resid, dY, dX1) {
  aic <- matrix(NA, nrow = 3, ncol = 3)
  bic <- matrix(NA, nrow = 3, ncol = 3)
  
  for (q in 1:3) {
    for (p in 1:3) {  
      # Construct the model formula properly
      formula <- as.formula(
        paste("dY ~ lag_resid +", 
              paste0("stats::lag(dY, -", 1:p, ")", collapse = " + "), " +",
              paste0("stats::lag(dX1, -", 0:q, ")", collapse = " + "))
      )
      
      # Estimate model
      model <- tryCatch(dynlm(formula, data = data), error = function(e) NULL)
      
      # Store AIC/BIC if model estimation was successful
      if (!is.null(model)) {
        aic[p, q] <- AIC(model)
        bic[p, q] <- BIC(model)
      }
    }
  }
  
  # Find best (p, q) based on AIC
  min_position <- which(aic == min(aic, na.rm = TRUE), arr.ind = TRUE)
  best_p <- min_position[1, 1]
  best_q <- min_position[1, 2]
  
  # Estimate the optimal model
  optimal_formula <- as.formula(
    paste("dY ~ lag_resid +", 
          paste0("stats::lag(dY, -", 1:best_p, ")", collapse = " + "), " +",
          paste0("stats::lag(dX1, -", 0:best_q, ")", collapse = " + "))
  )
  
  optimal_model <- dynlm(optimal_formula, data = data)
  
  return(optimal_model)
}
