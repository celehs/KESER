#' @title Main function for local feature selection
#' @param X_full ...
#' @param Y_full ...
#' @param X_train ...
#' @param Y_train ...
#' @param X_valid ...
#' @param Y_valid ...
#' @param alpha ...
#' @param lambda_lst ...
#' @param up_rate ...
#' @param drop_rate ...
#' @param cos_cut ...
#' @param add.ridge ...
#' @export
loc.feature.selection <- function(
  X_full, Y_full, X_train, Y_train, X_valid, Y_valid,
  alpha = 1, lambda_lst = NULL, up_rate = 3, 
  drop_rate = 0.5, cos_cut = 0.1, add.ridge = TRUE) {
  ################ cut by cosine ################
  cos_lst <- unlist(lapply(c(1:ncol(X_full)), function(j) {
    crossprod(Y_full, X_full[, j]) / sqrt(crossprod(Y_full) * crossprod(X_full[, j]))
  }))
  local_use_set <- which(abs(cos_lst) >= cos_cut)
  if (length(local_use_set) <= 1) {
    return(local_use_set)
  }
  X_full <- X_full[, local_use_set]
  X_train <- X_train[, local_use_set]
  X_valid <- X_valid[, local_use_set]
  ################ up-sample and dropout ################
  drop_m_full <- drop_fun(X_full, Y_full, drop_rate = drop_rate, up_rate = up_rate)
  drop_m_train <- drop_fun(X_train, Y_train, drop_rate = drop_rate, up_rate = up_rate)
  X_full <- drop_m_full$X
  Y_full <- drop_m_full$Y
  X_train <- drop_m_train$X
  Y_train <- drop_m_train$Y
  ################ Specify lambda_lst if it is NULL ################
  p1 <- length(X_full)
  n1 <- length(X_full)
  if (add.ridge == TRUE) {
    ridge.results <- add.ridge.fast(X_train, Y_train, X_valid, Y_valid, X_full, Y_full)
    Y_train <- ridge.results$Y_train
    Y_full <- ridge.results$Y_full
  }
  ################ Run CV-lasso ################
  fit.local <- Lasso_select_drop(
    X_train, Y_train, X_valid, Y_valid, X_full, Y_full, lambda_lst = lambda_lst, alpha = alpha)
  beta_loc <- data.frame(name = colnames(X_full), coef = fit.local$min.coef)
  list(lambda = fit.local$min.lambda, 
       results = data.table::data.table(beta_loc))
}
