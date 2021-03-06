# Used for adding ridge penalty to the covariates

add.ridge.fast <- function(X, Y, X_valid, Y_valid, X_all, Y_all){
  p <- ncol(X)
  n <- nrow(X)
  #min.lambda <- NULL
  #min.coef <- NULL
  #min.mse <- Inf
  #lambda_lst <- 1e-3 * c(c(1:400) * 0.01, c(41:500) * 0.1) * sd(Y) * sqrt(log(p) / n)
  
  fit_result <- cv.glmnet(X, Y, intercept = F, alpha = 0, standardize = F)
  min.lambda <- fit_result$lambda.min
  
  fit_result_all <- glmnet(X_all, Y_all, lambda = min.lambda, intercept = F,
                           alpha = 0, standardize = F)
  fit_result_train <- glmnet(X, Y, lambda = min.lambda, intercept = F,
                             alpha = 0, standardize = F)
  
  list(Y_full = predict(fit_result_all, X_all), 
       Y_train = predict(fit_result_train, X))
}

# Dropout and up-sample

drop_fun <- function(X, Y, drop_rate = 0.5, up_rate = 10){
  boot_ind <- sample(1:nrow(X), up_rate * nrow(X), replace = T)
  Y_new = Y[boot_ind]
  X_new = X[boot_ind,]
  cor_mat <- cor(X)
  p <- ncol(X)
  drop.gaussian <- mvrnorm(n = nrow(X_new), mu = rep(0, p), Sigma = cor_mat)
  drop.ind <- ifelse(drop.gaussian > 0, 1, 0)
  
  X_new <- drop.ind * X_new + (1 - drop.ind) * (rep(1, nrow(X_new)) %*% t(colMeans(X_new)))
  return(list(X = X_new, Y = Y_new))
}


# Fit local lasso with the validation set to tune

Lasso_select_drop <- function(X, Y, X_valid, Y_valid, X_all, Y_all, lambda_lst = NULL,
                              alpha = 1){

  ########################## Find scale of lambda ##########################
  
  if (is.null(lambda_lst)){
    fit.cv <- cv.glmnet(X, Y, intercept = F, alpha = alpha, standardize = F)
    lambda.min <- fit.cv$lambda.min
    lambda_lst = c(1e-3 * (1:999), 1:1000) * lambda.min
  }
  ############################################################################
  
  
  fit_result <- glmnet(X, Y, lambda = lambda_lst, intercept = F, alpha = alpha,
                       standardize = F)
  
  min.lambda <- NULL
  min.coef <- NULL
  min.mse <- Inf
  for (l in 1:length(lambda_lst)) {
    lambda <- fit_result$lambda[l]
    fit_coef <- as.vector(fit_result$beta[,l])
    #print(length(which(fit_coef != 0)))
    mse <- mean((Y_valid - as.matrix(X_valid) %*% fit_coef)^2)
    if (mse < min.mse){
      min.lambda <- lambda
      min.mse <- mse
      min.coef <- fit_coef
    }
    #print(mse)
  }
  
  fit_result_all <- glmnet(X_all, Y_all, lambda = min.lambda, intercept = F,
                           alpha = alpha, standardize = F)
  min.coef <- as.vector(fit_result_all$beta)
  
  return(list(min.lambda = min.lambda, min.coef = min.coef, min.mse = min.mse))
}


# Fit integrative group lasso with the validation set to tune

# Main function for local feature selection

loc.feature.selection <- function(X_full, Y_full, X_train, Y_train, X_valid, Y_valid,
                                  alpha = 1, lambda_lst = NULL, up_rate = 10, 
                                  drop_rate = 0.5, cos_cut = 0.1, add.ridge = T){
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

  if (add.ridge == T){
    ridge.results <- add.ridge.fast(X_train, Y_train, X_valid, Y_valid, X_full, Y_full)
    Y_train <- ridge.results$Y_train
    Y_full <- ridge.results$Y_full
  }

  ################ Run CV-lasso ################
  
  fit.local <- Lasso_select_drop(X_train, Y_train, X_valid, Y_valid, X_full, Y_full,
                                 lambda_lst = lambda_lst, alpha = alpha)
  beta_loc <- fit.local$min.coef
  beta_loc <- as.data.frame(beta_loc)
  rownames(beta_loc) <- colnames(X_full)
  return(list(min.lambda = fit.local$min.lambda, min.beta = beta_loc))
}

# Main function for integrative feature selection



int.feature.selection.fast <- function(X_full_lst, Y_full_lst, X_train_lst,
                                       Y_train_lst, X_valid_lst, Y_valid_lst,
                                       lambda_lst = NULL, add.ridge = T){
  
  ################# Drop-out #################
  
  n1_valid <- length(X_valid_lst[[1]][,1]); n2_valid <- length(X_valid_lst[[2]][,1])
  
  for (m in 1:2) {
    drop_m_full <- drop_fun(X_full_lst[[m]], Y_full_lst[[m]], 
                            drop_rate = 0.5, up_rate = 10)
    X_full_lst[[m]] <- drop_m_full$X
    Y_full_lst[[m]] <- drop_m_full$Y
    drop_m_train <- drop_fun(X_train_lst[[m]], Y_train_lst[[m]], 
                             drop_rate = 0.5, up_rate = 10)
    X_train_lst[[m]] <- drop_m_train$X
    Y_train_lst[[m]] <- drop_m_train$Y
  }
  
  p1 <- length(X_full_lst[[1]][1,]); p2 <- length(X_full_lst[[2]][1,])
  n1 <- length(X_full_lst[[1]][,1]); n2 <- length(X_full_lst[[2]][,1])
  
  ################# Add-ridge #################
  if (add.ridge){
    for (m in 1:2) {
      ridge.results <- add.ridge.fast(X_train_lst[[m]], Y_train_lst[[m]],
                                      X_valid_lst[[m]], Y_valid_lst[[m]],
                                      X_full_lst[[m]], Y_full_lst[[m]])
      Y_train_lst[[m]] <- ridge.results$Y_train
      Y_full_lst[[m]] <- ridge.results$Y_full
    }
  }

  var_set_all <- union(colnames(X_full_lst[[1]]), colnames(X_full_lst[[2]]))
  p_tot <- 2 * length(var_set_all)
  X_full_all <- matrix(0, n1 + n2, p_tot)
  X_train_all <- matrix(0, n1 + n2, p_tot)
  X_valid_all <- matrix(0, n1_valid + n2_valid, p_tot)
  
  for (m in 1:2) {
    match_m <- match(var_set_all, colnames(X_full_lst[[m]]))
    
    if (m == 1){
      X_full_all[1:n1, 2 * which(!is.na(match_m)) - 1] <- X_full_lst[[m]][,match_m[which(!is.na(match_m))]]
      X_train_all[1:n1, 2 * which(!is.na(match_m)) - 1] <- X_train_lst[[m]][,match_m[which(!is.na(match_m))]]
      X_valid_all[1:n1_valid, 2 * which(!is.na(match_m)) - 1] <- X_valid_lst[[m]][,match_m[which(!is.na(match_m))]]
    }
    if (m == 2){
      X_full_all[(n1 + 1):(n1 + n2), 2 * which(!is.na(match_m))] <- X_full_lst[[m]][,match_m[which(!is.na(match_m))]]
      X_train_all[(n1 + 1):(n1 + n2), 2 * which(!is.na(match_m))] <- X_train_lst[[m]][,match_m[which(!is.na(match_m))]]
      X_valid_all[(n1_valid + 1):(n1_valid + n2_valid), 2 * which(!is.na(match_m))] <- X_valid_lst[[m]][,match_m[which(!is.na(match_m))]]
    }
  }
  
  common_set <- intersect(colnames(X_full_lst[[1]]), colnames(X_full_lst[[2]]))
  common_indx <- which(var_set_all %in% common_set)
  
  Y_full_all <- c(Y_full_lst[[1]], Y_full_lst[[2]])
  Y_train_all <- c(Y_train_lst[[1]], Y_train_lst[[2]])
  Y_valid_all <- c(Y_valid_lst[[1]], Y_valid_lst[[2]])
  
  
  ############# Creat group index and penalty factor to fit for gglasso() #############
  
  group_ind_vec <- c(1:p_tot)
  group_ind_vec[2 * c(1:(p_tot / 2)) - 1] <- (group_ind_vec[2 * c(1:(p_tot / 2)) - 1] + 1) / 2
  group_ind_vec[2 * c(1:(p_tot / 2))] <- group_ind_vec[2 * c(1:(p_tot / 2))] / 2
  pf_group <- rep(1, (p_tot / 2))
  pf_group[which(c(1:(p_tot / 2)) %in% common_indx)] <- 1 / sqrt(2)
  
  ########################## Find scale of lambda ##########################
  
  if (is.null(lambda_lst)){
    fit.cv <- cv.gglasso(x = as.matrix(X_train_all), y = Y_train_all, group = group_ind_vec, 
                         pf = pf_group, intercept = F)
    lambda.min <- fit.cv$lambda.min
    lambda_lst = c(1e-3 * (1:999), 1:1000) * lambda.min
  }
  
  ############################################################################
  
  fit_result <- gglasso(x = as.matrix(X_train_all), y = Y_train_all, group = group_ind_vec, 
                        lambda = lambda_lst, pf = pf_group, intercept = F)
  
  min.lambda <- NULL
  min.coef <- NULL
  min.sse <- Inf
  
  XTX <- t(X_valid_all) %*% X_valid_all
  XTY <- t(X_valid_all) %*% Y_valid_all
    
  for (t in 1:length(fit_result$lambda)) {
    fit_coef <- fit_result$beta[,t]
    sse <- c(t(fit_coef) %*% XTX %*% fit_coef - 2 * t(XTY) %*% fit_coef)
    #print(sse)
    #print(length(which(fit_coef != 0)))
    
    if (sse < min.sse){
      min.lambda <- fit_result$lambda[t]
      min.sse <- sse
      min.coef <- fit_coef
    }
  }
  
  ############# bind back to form beta according to the original index #############
  
  fit_result <- gglasso(x = as.matrix(X_full_all), y = Y_full_all, group = group_ind_vec, 
                        lambda = min.lambda, pf = pf_group, intercept = F)
  fit_coef <- fit_result$beta
  beta_lst <- vector('list', 2)
  for (m in 1:2){
    match_m <- match(var_set_all, colnames(X_full_lst[[m]]))
    beta_lst[[m]] <- rep(0, ncol(X_full_lst[[m]]))
    beta_lst[[m]][match_m[which(!is.na(match_m))]] <- fit_coef[2 * which(!is.na(match_m)) + m - 2]
    beta_lst[[m]] <- as.data.frame(beta_lst[[m]])
    rownames(beta_lst[[m]]) <- colnames(X_full_lst[[m]])
  }
  beta.tab <- merge(beta_lst[[1]], beta_lst[[2]], by = 'row.names', all = T)
  rownames(beta.tab) <- beta.tab$Row.names
  beta.tab$Row.names <- NULL
  colnames(beta.tab) <- c('Study_1', 'Study_2')
  
  return(list(min.beta = beta.tab, min.lambda = min.lambda))
}




"
# Used for adding ridge penalty to the covariates

add_ridge <- function(X_1, Y_1, lambda, alpha = 0.5){
n <- nrow(X_1)
p <- ncol(X_1)
Cov_all <- t(cbind(X_1, Y_1)) %*% cbind(X_1, Y_1)
Cov_all <- diag(lambda * alpha * c(rep(1, p), 0)) + Cov_all
svd_result <- svd(Cov_all)
s_value <- svd_result$d
s_mat <- diag(sqrt(s_value))
data_all <- svd_result$u %*% s_mat
X <- t(data_all[-length(data_all[ ,1]),])
Y <- data_all[length(data_all[ ,1]),]
return(list(X = X, Y = Y))
}


Combine_fit_drop <- function(X_lst, Y_lst, X_lst_valid, Y_lst_valid, common_indx, 
lambda_M_lst, lambda_coef_lst, lambda_g_lst, alpha = 0.5){
M <- length(Y_lst)
options(warn = -1)

p_lst <- c()
for (m in 1:M) {
p_lst <- c(p_lst, length(X_lst[[m]][1, ]))
}

############# Creat group index #############

indx_lst <- c(1:length(X_lst[[1]][1, ]))

for (m in 2:M){ 
indx_m <- 1:(p_lst[m]) + sum(p_lst[1:(m - 1)])
indx_m[common_indx[[m]]] <- c(1:p_lst[1])[common_indx[[1]]]
indx_lst <- c(indx_lst, indx_m)
}

for (t in 2:length(indx_lst)){
if (length(which(indx_lst == indx_lst[t])) == 1){
indx_lst[t] <- max(indx_lst[1:(t - 1)]) + 1
}
}

############# Add ridge penalty and bind X together for as a matrix #############

X_1 <- X_lst[[1]]
Y_1 <- Y_lst[[1]]

data_ridge <- add_ridge(X_1, Y_1, lambda_M_lst[1], alpha = alpha)

X_all <- data_ridge$X
Y_all <- data_ridge$Y
X_all_valid <- X_lst_valid[[1]]
Y_all_valid <- Y_lst_valid[[1]]

for (m in 2:M){
X_m <- X_lst[[m]]
Y_m <- Y_lst[[m]]
data_ridge <- add_ridge(X_m, Y_m, lambda_M_lst[m], alpha = alpha)
X_all <- bdiag(X_all, data_ridge$X)
Y_all <- c(Y_all, data_ridge$Y)
X_all_valid <- bdiag(X_all_valid, X_lst_valid[[m]])
Y_all_valid <- c(Y_all_valid, Y_lst_valid[[m]])
}

lambda_M_lst <- lambda_M_lst / nrow(X_all)
lambda <- mean(lambda_M_lst)
group_id <- 1:length(unique(indx_lst))

# tune with SSE
min.lambda <- NULL
min.coef <- NULL
min.sse <- Inf

penalty_factor <- c()
group_ind_vec <- c()
indx_convert <- c()

############# Creat group index and penalty factor to fit for gglasso() #############

for (t in 1:length(group_id)){
if (length(which(indx_lst == group_id[t])) == 2){
penalty_factor <- c(penalty_factor, 3)
group_ind_vec <- c(group_ind_vec, c(group_id[t], group_id[t]))
}else{
if (group_id[t] <= p_lst[1]){
penalty_factor <- c(penalty_factor, 1)
}else{
penalty_factor <- c(penalty_factor, 2)
}
group_ind_vec <- c(group_ind_vec, group_id[t])
}
indx_convert <- c(indx_convert, which(indx_lst == group_id[t]))
}
X_all <- X_all[,indx_convert]
X_all_valid <- X_all_valid[,indx_convert]

for (lambda_coef in lambda_coef_lst) {
for (lambda_g in lambda_g_lst){

penalty_fac_lam <- penalty_factor
penalty_fac_lam[which(penalty_fac_lam == 1)] <- lambda_M_lst[1] / lambda
penalty_fac_lam[which(penalty_fac_lam == 2)] <- lambda_M_lst[2] / lambda
penalty_fac_lam[which(penalty_fac_lam == 3)] <- lambda_g
penalty_fac_lam <- lambda_coef * penalty_fac_lam

fit_result <- gglasso(x = as.matrix(X_all), y = Y_all, group = group_ind_vec, 
lambda = lambda, pf = penalty_fac_lam, intercept = F)
fit_coef <- fit_result$beta

sse <- sum((Y_all_valid - X_all_valid %*% fit_coef)^2)
print(sse)
print(length(which(fit_coef != 0)))

if (sse < min.sse){
min.lambda <- c(lambda_coef, lambda_g)
min.sse <- sse
min.coef <- fit_coef
}

}
}

############# bind back to form beta according to the original index #############

coef_final <- rep(NA, length(min.coef))
for (k in 1:length(indx_convert)) {
coef_final[indx_convert[k]] <- min.coef[k]
}

beta_lst <- vector('list', M)
pos.cut <- 1
for (m in 1:M) {
pos.end <- pos.cut + p_lst[m] - 1
beta_lst[[m]] <- coef_final[pos.cut:pos.end]
pos.cut <- pos.end + 1
}

return(list(min.lambda = min.lambda, min.beta = beta_lst, min.sse = sse))
}



int.feature.selection <- function(X_full_lst, Y_full_lst, X_train_lst,
                                  Y_train_lst, X_valid_lst, Y_valid_lst,
                                  alpha = 0.5, lambda_single_lst = NULL, 
                                  lambda_group_lst = NULL, 
                                  up.rate = 10, drop.rate = 0.5, cos.cut = 0.1){
  
  ########## Find the set of covariates shared by the two studies ##########
  
  common_set <- intersect(colnames(X_full_lst[[1]]), colnames(X_full_lst[[2]]))
  common_indx <- vector('list', 2)
  for (variable in common_set) {
    common_indx[[1]] <- c(common_indx[[1]], which(colnames(X_full_lst[[1]]) == variable))
    common_indx[[2]] <- c(common_indx[[2]], which(colnames(X_full_lst[[2]]) == variable))
  }
  
  
  ######### dropout and up-sample ############
  
  p1 <- length(X_full_lst[[1]][1,])
  p2 <- length(X_full_lst[[2]][1,])
  n1 <- length(X_full_lst[[1]][,1])
  n2 <- length(X_full_lst[[2]][,1])
  
  for (m in 1:2) {
    drop_m_full <- drop_fun(X_full_lst[[m]], Y_full_lst[[m]], drop_rate = 0.5, up_rate = 10)
    X_full_lst[[m]] <- drop_m_full$X
    Y_full_lst[[m]] <- drop_m_full$Y
    drop_m_train <- drop_fun(X_train_lst[[m]], Y_train_lst[[m]], drop_rate = 0.5, up_rate = 10)
    X_train_lst[[m]] <- drop_m_train$X
    Y_train_lst[[m]] <- drop_m_train$Y
  }
  
  ######### Fit two local regression to specify the proportion of lambda ###########
  ######### Aim of this is to safe the time from tuning too many parameters ########
  
  fit.local1 <- Lasso_select_drop(X_train_lst[[1]], Y_train_lst[[1]], X_valid_lst[[1]], 
                                  Y_valid_lst[[1]], X_full_lst[[1]], Y_full_lst[[1]],
                                  lambda_lst = 0.3 * c(c(1:400) * 0.0003, c(41:400) * 0.003, c(41:1500) * 0.03) *
                                    sd(Y_train_lst[[1]]) * sqrt(log(p1) / n1), alpha = alpha)
  fit.local2 <- Lasso_select_drop(X_train_lst[[2]], Y_train_lst[[2]], X_valid_lst[[2]],
                                  Y_valid_lst[[2]], X_full_lst[[2]], Y_full_lst[[2]], 
                                  lambda_lst = 0.4 * c(c(1:400) * 0.001, c(81:200) * 0.005, c(51:1800) * 0.02) *
                                    sd(Y_train_lst[[2]]) * sqrt(log(p2) / n2), alpha = alpha)
  if (is.null(lambda_single_lst)){
    lambda_single_lst <- 0.0005 * c(1:200)
  }
  if (is.null(lambda_group_lst)){
    lambda_group_lst <- 0.75 * c(c(5:25) * 0.02, c(7:300) * 0.08)
  }
  
  ######### Tuning parameter ###########
  
  fit.grp <- Combine_fit_drop(X_train_lst, Y_train_lst, X_valid_lst, Y_valid_lst, common_indx, 
                              lambda_M_lst = 4 / (sd(Y_train_lst[[1]]) + sd(Y_train_lst[[2]])) * 
                                c(length(Y_train_lst[[1]]) * fit.local1$min.lambda, 
                                  length(Y_train_lst[[2]]) * fit.local2$min.lambda),
                              lambda_coef_lst = lambda_single_lst,
                              lambda_g_lst = lambda_group_lst)
  
  ######### Train with the tuned parameters ###########
  
  fit.grp.full <- Combine_fit_drop(X_full_lst, Y_full_lst, X_valid_lst, Y_valid_lst, common_indx, 
                                   lambda_M_lst = 4 / (sd(Y_train_lst[[1]]) + sd(Y_train_lst[[2]])) * 
                                     c(length(Y_train_lst[[1]]) * fit.local1$min.lambda, 
                                       length(Y_train_lst[[2]]) * fit.local2$min.lambda),
                                   lambda_coef_lst = fit.grp$min.lambda[1],
                                   lambda_g_lst = fit.grp$min.lambda[2])
  
  beta_lst <- fit.grp.full$min.beta
  
  for (m in 1:2){
    beta_lst[[m]] <- as.data.frame(beta_lst[[m]])
    rownames(beta_lst[[m]]) <- colnames(X_full_lst[[m]])
  }
  return(list(min.lambda = fit.grp$min.lambda, min.beta = beta_lst))
  
}

"