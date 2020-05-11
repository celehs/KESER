# Used for adding ridge penalty to the covariates
# UNUSED ARGUMENTS: X_valid, Y_valid
add.ridge.fast <- function(X, Y, X_valid, Y_valid, X_all, Y_all) {
  fit_result <- glmnet::cv.glmnet(
    X, Y, intercept = FALSE, standardize = FALSE, alpha = 0)
  min.lambda <- fit_result$lambda.min
  fit_result_train <- glmnet::glmnet(
    X, Y, intercept = FALSE, standardize = FALSE, alpha = 0, lambda = min.lambda)  
  fit_result_all <- glmnet::glmnet(
    X_all, Y_all, intercept = FALSE, standardize = FALSE, alpha = 0, lambda = min.lambda)  
  list(Y_full = glmnet::predict.glmnet(fit_result_all, X_all), 
       Y_train = glmnet::predict.glmnet(fit_result_train, X))
}
