# Fit local lasso with the validation set to tune
Lasso_select_drop <- function(
  X, Y, X_valid, Y_valid, X_all, Y_all, lambda_lst, alpha = 0.5) {
  min.lambda <- NULL
  min.coef <- NULL
  min.mse <- Inf
  fit_result <- glmnet::glmnet(
    X, Y, lambda = lambda_lst, alpha = alpha, 
    intercept = FALSE, standardize = FALSE)
  for (l in 1:length(lambda_lst)) {
    lambda <- fit_result$lambda[l]
    fit_coef <- as.vector(fit_result$beta[,l])
    mse <- mean((Y_valid - as.matrix(X_valid) %*% fit_coef)^2)
    if (mse < min.mse){
      min.lambda <- lambda
      min.mse <- mse
      min.coef <- fit_coef
    }
  }
  fit_result_all <- glmnet::glmnet(
    X_all, Y_all, lambda = min.lambda, alpha = alpha, 
    intercept = FALSE, standardize = FALSE)
  min.coef <- as.vector(fit_result_all$beta)
  list(min.lambda = min.lambda, min.coef = min.coef, min.mse = min.mse)
}
