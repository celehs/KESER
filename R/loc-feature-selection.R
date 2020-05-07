# Dropout and up-sample
drop_fun <- function(X, Y, drop_rate = 0.5, up_rate = 10) {
  boot_ind <- sample(1:nrow(X), up_rate * nrow(X), replace = T)
  Y_new = Y[boot_ind]
  X_new = X[boot_ind,]
  cor_mat <- cor(X)
  p <- ncol(X)
  drop.gaussian <- MASS::mvrnorm(n = nrow(X_new), mu = rep(0, p), Sigma = cor_mat)
  drop.ind <- ifelse(drop.gaussian > 0, 1, 0)
  
  X_new <- drop.ind * X_new + (1 - drop.ind) * (rep(1, nrow(X_new)) %*% t(colMeans(X_new)))
  return(list(X = X_new, Y = Y_new))
}

# Fit local lasso with the validation set to tune
Lasso_select_drop <- function(X, Y, X_valid, Y_valid, X_all, Y_all, lambda_lst,
                              alpha = 0.5) {
  min.lambda <- NULL
  min.coef <- NULL
  min.mse <- Inf
  
  fit_result <- glmnet::glmnet(X, Y, lambda = lambda_lst, intercept = F, alpha = alpha,
                       standardize = F)
  
  for (l in 1:length(lambda_lst)) {
    lambda <- fit_result$lambda[l]
    fit_coef <- as.vector(fit_result$beta[,l])
    print(length(which(fit_coef != 0)))
    mse <- mean((Y_valid - as.matrix(X_valid) %*% fit_coef)^2)
    if (mse < min.mse){
      min.lambda <- lambda
      min.mse <- mse
      min.coef <- fit_coef
    }
    print(mse)
  }
  
  fit_result_all <- glmnet::glmnet(X_all, Y_all, lambda = min.lambda, intercept = F,
                           alpha = alpha, standardize = F)
  min.coef <- as.vector(fit_result_all$beta)
  
  return(list(min.lambda = min.lambda, min.coef = min.coef, min.mse = min.mse))
}


# Main function for local feature selection
loc.feature.selection <- function(X_full, Y_full, X_train, Y_train, X_valid, Y_valid,
                                  alpha = 0.5, lambda_lst = NULL, up_rate = 10, 
                                  drop_rate = 0.5, cos_cut = 0.1) {
  ################ cut by cosine ################
  
  cos_lst <- unlist(lapply(c(1:ncol(X_full)),function(j) {
    crossprod(Y_full, X_full[,j]) / sqrt(crossprod(Y_full) *crossprod(X_full[,j]))}))
  local_use_set <- which(abs(cos_lst) >= cos_cut)
  if (length(local_use_set) <= 1){
    return(local_use_set)
  }
  
  X_full <- X_full[,local_use_set]
  X_train <- X_train[,local_use_set]
  X_valid <- X_valid[,local_use_set]
  
  ################ up-sample and dropout ################
  
  drop_m_full <- drop_fun(X_full, Y_full, drop_rate = drop_rate, up_rate = up_rate)
  X_full <- drop_m_full$X
  Y_full <- drop_m_full$Y
  
  drop_m_train <- drop_fun(X_train, Y_train, drop_rate = drop_rate, up_rate = up_rate)
  X_train <- drop_m_train$X
  Y_train <- drop_m_train$Y
  
  ################ Specify lambda_lst if it is NULL ################
  
  p1 <- length(X_full)
  n1 <- length(X_full)
  if (is.null(lambda_lst)){
    lambda_lst = 0.3 * c(c(1:400) * 0.0003, c(41:400) * 0.003, 
                         c(41:1500) * 0.03) * sd(Y_train) * sqrt(log(p1) / n1)
  }
  
  ################ Run CV-lasso ################
  
  fit.local <- Lasso_select_drop(X_train, Y_train, X_valid, Y_valid, X_full, Y_full,
                                 lambda_lst = lambda_lst, alpha = alpha)
  beta_loc <- fit.local$min.coef
  beta_loc <- as.data.frame(beta_loc)
  rownames(beta_loc) <- colnames(X_full)
  return(list(min.lambda = fit.local$min.lambda, min.beta = beta_loc))
}

