library(dynlm)

#Esta funcion da los resultados del equilibrio de largo plazo
long_run_results_function <- function(model,data,sample){
  print(summary(model))
  
  ecm_term <- residuals(model)
  
  
  fig <- plot_ly(y=ecm_term%>%as.numeric(),x=index(data), type = 'scatter', mode = 'lines')%>%
    layout(title=paste("Long-run disequilibrium",sample ))
  print(fig)
  
  
  print(adf.test(ecm_term))
  print(pp.test(ecm_term))
  print(kpss.test(ecm_term))
  
  return(ecm_term)
  
}


#Esta funcion estima el modelo de dinamica de corto plazo selecccionando los lags segun minimicen el criterio AIC y evitando overfitting
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



# Function to estimate threshold ECM and calculate AIC
estimate_tECM_AIC <- function(resid, p_max) {
  # Prepare differenced residuals and lagged residuals
  du <- diff(resid)
  lagu <- stats::lag(resid, -1)
  I_t <- ifelse(lagu >= 0, 1, 0)
  
  # Prepare lagged Δu terms
  du_lags <- lapply(1:p_max, function(i) lag_vec(du, i))
  du_lags <- do.call(cbind, du_lags)
  
  # Create data frame for fitting models
  df <- na.omit(data.frame(
    dy = du,
    lagu = lagu[2:length(resid)],
    I = I_t[2:length(resid)],
    du_lags
  ))
  
  # Initialize AIC storage
  aic_values <- numeric(p_max)
  
  # Loop over lag choices
  for (p in 1:p_max) {
    # Create ECT terms for both regimes
    df$ECT_pos <- df$I * df$lagu
    df$ECT_neg <- (1 - df$I) * df$lagu
    
    # Fit model with p lags (AR terms are common across regimes)
    formula <- as.formula(paste("dy ~ ECT_pos + ECT_neg +", paste(paste0("X", 1:p), collapse = " + ")))
    model <- lm(formula, data = df)
    
    # Compute AIC
    aic_values[p] <- AIC(model)
  }
  
  # Return AIC values and optimal lag
  list(aic_values = aic_values, optimal_p = which.min(aic_values))
}

data_frame_tar_model <- function(resid,p) {
  # Prepare differenced residuals and lagged residuals
  du <- diff(resid)
  lagu <- lag_vec(resid, 1)
  I_t <- ifelse(lagu >= 0, 1, 0)
  
  # Prepare lagged Δu terms
  du_lags <- lapply(1:p, function(i) lag_vec(du, i))
  du_lags <- do.call(cbind, du_lags)
  
  # Create data frame for fitting models
  df <- na.omit(data.frame(
    dy = du,
    lagu = lagu[2:length(resid)],
    I = I_t[2:length(resid)],
    du_lags
  ))
  return(df)}

tar_model_estimation <- function(resid,p_max){
  
  # Example: Estimate AIC for p from 1 to 5 lags
  result <- estimate_tECM_AIC(resid, p_max = 3)
  
  # Display AIC values and the optimal p
  result$aic_values
  result$optimal_p
  
  # Fit the optimal model with the best lag (p*)
  optimal_p <- result$optimal_p
  
  #data frame TAR model
  df <- data_frame_tar_model(resid,optimal_p)
  
  # Fit the threshold ECM model with the optimal p
  df$ECT_pos <- df$I * df$lagu
  df$ECT_neg <- (1 - df$I) * df$lagu
  
  # Create formula for the model with the optimal p
  formula <- as.formula(paste("dy ~ ECT_pos + ECT_neg +", paste(paste0("X", 1:optimal_p), collapse = " + ")))
  
  # Fit the final model
  tar_model <- lm(formula, data = df)
  
return(tar_model)}


tar_optimal_short_run_model_function <- function(data, lag_resid, dY, dX1) {
  aic <- matrix(NA, nrow = 3, ncol = 3)
  bic <- matrix(NA, nrow = 3, ncol = 3)
  
  I_t <- ifelse(lag_resid >= 0, 1, 0)
  
  
  # Fit the threshold ECM model with the optimal p
  ECT_pos <- I_t * lag_resid
  ECT_neg <- (1 - I_t) * lag_resid
  
  for (q in 1:3) {
    for (p in 1:3) {  
      # Construct the model formula properly
      formula <- as.formula(
        paste("dY ~ ECT_pos+ ECT_neg+", 
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
    paste("dY ~ ECT_pos+ ECT_neg +", 
          paste0("stats::lag(dY, -", 1:best_p, ")", collapse = " + "), " +",
          paste0("stats::lag(dX1, -", 0:best_q, ")", collapse = " + "))
  )
  
  optimal_model <- dynlm(optimal_formula, data = data)
  
  return(optimal_model)
}


simulate_restoration_time_ci <- function(alpha_hat, se, n_sim = 10000) {
  # Function to compute 95%-life (restoration time)
  restoration_time <- function(alpha) {
    log(0.05) / log(1 + alpha)
  }
  
  # Simulate alpha draws
  set.seed(123)  # for reproducibility
  alpha_draws <- rnorm(n_sim, mean = alpha_hat, sd = se)
  
  # Keep only stable draws: alpha in (-1, 0)
  alpha_draws <- alpha_draws[alpha_draws > -1 & alpha_draws < 0]
  
  # Compute restoration time for each draw
  t_draws <- restoration_time(alpha_draws)
  
  # Calculate point estimate and 95% CI
  point_est <- restoration_time(alpha_hat)
  ci <- quantile(t_draws, probs = c(0.025, 0.975), na.rm = TRUE)
  
  list(
    point_estimate = point_est,
    CI_95 = ci,
    draws_retained = length(alpha_draws),
    distribution_periods = t_draws
  )
}

