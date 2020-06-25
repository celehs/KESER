# Fit local lasso with the validation set to tune
Lasso_select_drop <- function(
  X, Y, X_valid, Y_valid, X_all, Y_all, lambda_lst = NULL, alpha = 1) {
  ########################## Find scale of lambda ##########################
  if (is.null(lambda_lst)) {
    fit.cv <- glmnet::cv.glmnet(
      X, Y, intercept = FALSE, standardize = FALSE, alpha = alpha)
    lambda.min <- fit.cv$lambda.min
    lambda_lst <- exp(seq(log(1e-3 * lambda.min),
                          log(1e3 * lambda.min), length.out = 300))
  }
  ############################################################################
  fit_result <- glmnet::glmnet(
    X, Y, intercept = FALSE, standardize = FALSE, alpha = alpha, lambda = lambda_lst)
  min.lambda <- NULL
  min.coef <- NULL
  min.mse <- Inf
  for (l in 1:length(lambda_lst)) {
    lambda <- fit_result$lambda[l]
    fit_coef <- as.vector(fit_result$beta[, l])
    # print(length(which(fit_coef != 0)))
    mse <- mean((Y_valid - as.matrix(X_valid) %*% fit_coef)^2)
    if (mse < min.mse){
      min.lambda <- lambda
      min.mse <- mse
      min.coef <- fit_coef
    }
    # print(mse)
  }
  fit_result_all <- glmnet::glmnet(
    X_all, Y_all, intercept = FALSE, standardize = FALSE, alpha = alpha, lambda = min.lambda)
  min.coef <- as.vector(fit_result_all$beta)
  list(min.lambda = min.lambda, min.coef = min.coef, min.mse = min.mse)
}
