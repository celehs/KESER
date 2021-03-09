#X_full=X_full_local_lst[[1]]
#Y_full=Y_full_lst[[1]]
#X_train=X_train_local_lst[[1]]
#Y_train=Y_train_lst[[1]]
#X_valid=X_valid_local_lst[[1]]
#Y_valid=Y_valid_lst[[1]]


STD.fun = function(xx){xx=as.matrix(xx); xx=(xx-VTM(apply(xx,2,mean),nrow(xx)))/VTM(apply(xx,2,sd),nrow(xx)); if(ncol(xx)==1){xx=c(xx)}; xx}

cos.fun = function(xx, yy){xx=as.matrix(xx);yy=as.matrix(yy) 
cos.xx<- unlist(lapply(c(1:ncol(xx)),function(j) {
  crossprod(yy, xx[,j]) / sqrt(crossprod(yy) *crossprod(xx[,j]))}))
cos.xx[is.na(cos.xx)]=0
cos.xx
}

weight.fun = function(xx, yy){
cos.xx.abs=abs(cos.fun(xx,yy))
sqrt(max(cos.xx.abs)/cos.xx.abs)
}

weight.fun.joint = function(xx_lst, yy_lst){
  weight_loc_lst <- vector('list', 2)
  
  for (m in 1:2){
    weight_loc_lst[[m]] <- weight.fun(xx_lst[[m]], yy_lst[[m]])
  }
  
  for (m in 1:2) {
    p <- ncol(xx_lst[[m]])
    for (j in 1:p) {
      var_j <- colnames(xx_lst[[m]])[j]
      if (var_j %in% colnames(xx_lst[[3 - m]])){
        t <- which(colnames(xx_lst[[3 - m]]) == var_j)
        weight_loc_lst[[m]][j] <- min(weight_loc_lst[[m]][j], weight_loc_lst[[3 - m]][t])
      }
    }
  }
  return(weight_loc_lst)
}




# Used for adding ridge penalty to the covariates
add.ridge.lambda.modify=function(X_train, Y_train, X_valid, Y_valid, X_full, Y_full, 
                          weight_full, weight_train, weight_valid){
  nn=length(Y_full); pp=dim(X_full)[2]; lam.xx=svd(t(X_full)%*%X_full/nn)$d; 
  #lam.range0 = exp(seq(log(1e-6),log(10), length.out = 500))
  lam.range0 = exp(seq(log(1e-5),log(1e5), length.out = 1000))
  
  bini = glmnet(X_full,Y_full,alpha=0,lambda=lam.range0, penalty.factor=weight_full, intercept=F)
  tmpdf = apply(lam.xx/(lam.xx+VTM(bini$lambda,pp)),2,sum)
  lam.ridge.ini  = c("AIC" =bini$lambda[which.min(deviance(bini)+2*tmpdf)],
                     "BIC" =bini$lambda[which.min(deviance(bini)+log(nn)*tmpdf)],
                     "BICm"=bini$lambda[which.min(deviance(bini)+min(log(nn),nn^0.1)*tmpdf)])
  lam.range = seq(min(lam.ridge.ini)/2,max(lam.ridge.ini)*2,length=300)
  bini = glmnet(X_full,Y_full,alpha=0,lambda=lam.range, intercept=F)

  tmpdf = apply(lam.xx/(lam.xx+VTM(bini$lambda,pp)),2,sum)
  
  lam.ridge.select = c("AIC" =bini$lambda[which.min(deviance(bini)+2*tmpdf)], 
                       "BIC" =bini$lambda[which.min(deviance(bini)+log(nn)*tmpdf)],
                       "BICm"=bini$lambda[which.min(deviance(bini)+min(log(nn),nn^0.1)*tmpdf)])
  
  ##external validation using mse
  bini = glmnet(X_train,Y_train,alpha=0,lambda=lam.range0,penalty.factor=weight_train, intercept=F)
  mse <- apply(coef(bini), 2, function(fit_coef) mean((Y_valid-as.matrix(X_valid) %*% fit_coef[-1])^2))
  
  lam.range = seq(bini$lambda[which.min(mse)]/3,bini$lambda[which.min(mse)]*3,length=1000)
  bini = glmnet(X_train,Y_train,alpha=0,lambda=lam.range,penalty.factor=weight_train,intercept=F)
  mse <- apply(coef(bini), 2, function(fit_coef) mean((Y_valid-as.matrix(X_valid) %*% fit_coef[-1])^2))
  
  lam.ridge.select = c(lam.ridge.select, "MSE"=bini$lambda[which.min(mse)])
  lam.ridge.select
}

add.ridge.fast.modify <- function(X, Y, X_valid, Y_valid, X_all, Y_all,
                           lambda = 0, weight_full, weight_train, weight_valid){
  p <- ncol(X)
  n <- nrow(X)
  
  min.lambda <- lambda
  fit_result_all <- glmnet(X_all, Y_all, lambda = min.lambda, intercept = F,
                           alpha = 0, standardize = F,penalty.factor=weight_full)
  fit_result_train <- glmnet(X, Y, lambda = min.lambda, intercept = F,
                             alpha = 0, standardize = F,penalty.factor=weight_train)
  
  list(Y_full = predict(fit_result_all, X_all), 
       Y_train = predict(fit_result_train, X))
}

# Dropout and up-sample

drop_fun <- function(X, Y, drop_rate = 0.5, up_rate = 2){
  if(up_rate>1){
  boot_ind <- sample(1:nrow(X), up_rate * nrow(X), replace = T)
  Y_new = Y[boot_ind]
  X_new = X[boot_ind,]
  }else{Y_new=Y; X_new=X}
  cor_mat <- cor(X)
  p <- ncol(X)
  if(drop_rate!=0){
  drop.ind <- apply(X_new,2, function(ll) rbinom(n = length(ll), 1, drop_rate))
  
  X_new <- (1-drop.ind) * X_new + drop.ind * (rep(1, nrow(X_new)) %*% t(colMeans(X_new)))
  }
  return(list(X = X_new, Y = Y_new))
}



Lasso_select_drop.modify <- function(X, Y, X_valid, Y_valid, X_full, Y_full, lambda_lst = NULL,
                              alpha = 1,weight_full, weight_train, weight_valid){

  
  lambda_lst <- exp(seq(log(1e-5), log(1e5), length.out = 1000))
  fit_result <- glmnet(X, Y, lambda = lambda_lst, intercept = F, alpha = alpha,
                       standardize = F,penalty.factor=weight_train)
  
  min.lambda <- NULL
  min.coef <- NULL
  min.mse <- Inf
  for (l in 1:length(lambda_lst)) {
    lambda <- fit_result$lambda[l]
    fit_coef <- as.vector(fit_result$beta[,l])
    #print(length(fit_coef))
    mse <- mean((Y_valid - as.matrix(X_valid) %*% fit_coef)^2)
    if (mse < min.mse){
      min.lambda <- lambda
      min.mse <- mse
      min.coef <- fit_coef
    }
    
  }
  
  lambda_lst <- seq(min.lambda / 4, min.lambda * 4, length.out = 1000)
  fit_result <- glmnet(X, Y, lambda = lambda_lst, intercept = F, alpha = alpha,
                       standardize = F, penalty.factor=weight_train)
  min.lambda <- NULL
  min.coef <- NULL
  min.mse <- Inf
  for (l in 1:length(lambda_lst)) {
    lambda <- fit_result$lambda[l]
    fit_coef <- as.vector(fit_result$beta[,l])
    #print(length(fit_coef))
    mse <- mean((Y_valid - as.matrix(X_valid) %*% fit_coef)^2)
    if (mse < min.mse){
      min.lambda <- lambda
      min.mse <- mse
      min.coef <- fit_coef
    }
    
  }
  fit_result_all <- glmnet(X_full, Y_full, lambda = min.lambda, intercept = F,
                           alpha = alpha, standardize = F, penalty.factor=weight_full)
  min.coef <- as.vector(fit_result_all$beta)
  
  return(list(min.lambda = min.lambda, min.coef = min.coef, min.mse = min.mse))
}

# Fit integrative group lasso with the validation set to tune

# Main function for local feature selection

loc.feature.selection.new <- function(X_full, Y_full, X_train, Y_train, X_valid, Y_valid,
                                  alpha = 1, lambda_lst = NULL, up_rate = 2, 
                                  drop_rate = 0.5, add.ridge = T, ridge.tuning="BICm", yes.stand=1, yes.weight=1){

  ################ up-sample and dropout ################
  if(yes.stand==1){
    X_full = STD.fun(X_full); X_train = STD.fun(X_train)
    Y_full = STD.fun(Y_full); Y_train = STD.fun(Y_train)
    X_valid = STD.fun(X_valid); Y_valid = STD.fun(Y_valid)
  }
  
  "
  if(yes.weight==1){
    weight_full = weight.fun(X_full, Y_full)
    weight_train = weight.fun(X_train, Y_train)
    weight_valid = weight.fun(X_valid, Y_valid)
  }else{
    weight_full = rep(1, dim(X_full)[2])
    weight_train = rep(1, dim(X_train)[2])
    weight_valid = rep(1,dim(X_valid)[2])
  }
  "
  
  weight_full = rep(1, dim(X_full)[2])
  weight_train = rep(1, dim(X_train)[2])
  weight_valid = rep(1,dim(X_valid)[2])

  if(drop_rate!=0|up_rate>1){
  drop_m_full <- drop_fun(X_full, Y_full, drop_rate = drop_rate, up_rate = up_rate)
  X_full <- drop_m_full$X
  Y_full <- drop_m_full$Y
  
  drop_m_train <- drop_fun(X_train, Y_train, drop_rate = drop_rate, up_rate = up_rate)
  X_train <- drop_m_train$X
  Y_train <- drop_m_train$Y
  }
  ################ Specify lambda_lst if it is NULL ################
  
  p1 <- length(X_full)
  n1 <- length(X_full)

  if (add.ridge == T){
    lambda.ridge=add.ridge.lambda.modify(X_train, Y_train, X_valid, Y_valid,
                                         X_full, Y_full, weight_full, weight_train, 
                                         weight_valid)[ridge.tuning]

    ridge.results <- add.ridge.fast.modify(X_train, Y_train, X_valid, Y_valid, X_full, Y_full,
                                    lambda = lambda.ridge, weight_full, weight_train, weight_valid)
    Y_train <- ridge.results$Y_train
    Y_full <- ridge.results$Y_full
  }
  
  if(yes.weight==1){
    weight_full_X = weight.fun(X_full, Y_full)
    weight_train_X = weight.fun(X_train, Y_train)
    weight_valid_X = weight.fun(X_valid, Y_valid)
    
    X_full <- t(t(X_full) * weight_full_X^(-2))
    X_train <- t(t(X_train) * weight_train_X^(-2))
    X_valid <- t(t(X_valid) * weight_valid_X^(-2))
  }

  ################ Run CV-lasso ################
  
  fit.local <- Lasso_select_drop.modify(X_train, Y_train, X_valid, Y_valid, X_full, Y_full,
                                 lambda_lst = lambda_lst, alpha = alpha, weight_full, weight_train, weight_valid)
  beta_loc <- fit.local$min.coef
  beta_loc <- as.data.frame(beta_loc)
  rownames(beta_loc) <- colnames(X_full)
  return(list(min.lambda = fit.local$min.lambda, min.beta = beta_loc))
}

# Main function for integrative feature selection


int.feature.selection.fast.new <- function(X_full_lst, Y_full_lst, X_train_lst,
                                           Y_train_lst, X_valid_lst, Y_valid_lst,
                                           lambda_lst = NULL, add.ridge = T,
                                           drop_rate = 0.5, ridge.tuning="BICm", up_rate = 3, 
                                           yes.stand=1, yes.weight=1){
  
  if(yes.stand==1){
    for(type in 1:2){
      X_full_lst[[type]] = STD.fun(X_full_lst[[type]]); X_train_lst[[type]] = STD.fun(X_train_lst[[type]])
      Y_full_lst[[type]] = STD.fun(Y_full_lst[[type]]); Y_train_lst[[type]] = STD.fun(Y_train_lst[[type]])
      X_valid_lst[[type]] = STD.fun(X_valid_lst[[type]]); Y_valid_lst[[type]] = STD.fun(Y_valid_lst[[type]])
    }
  }
  n_vec <- rep(0, 0)
  n_vec[1] <- length(X_full_lst[[1]][,1]); n_vec[2] <- length(X_full_lst[[2]][,1])
  n_mean <- mean(n_vec)
  
  ################# Drop-out #################
  
  n1_valid <- length(X_valid_lst[[1]][,1]); n2_valid <- length(X_valid_lst[[2]][,1])
  
  if(drop_rate!=0|up_rate>1){
    for (m in 1:2) {
      drop_m_full <- drop_fun(X_full_lst[[m]], Y_full_lst[[m]], 
                              drop_rate = drop_rate, up_rate = up_rate)
      X_full_lst[[m]] <- drop_m_full$X
      Y_full_lst[[m]] <- drop_m_full$Y
      drop_m_train <- drop_fun(X_train_lst[[m]], Y_train_lst[[m]], 
                               drop_rate = drop_rate, up_rate = up_rate)
      X_train_lst[[m]] <- drop_m_train$X
      Y_train_lst[[m]] <- drop_m_train$Y
    }
  }
  
  p1 <- length(X_full_lst[[1]][1,]); p2 <- length(X_full_lst[[2]][1,])
  n1 <- length(X_full_lst[[1]][,1]); n2 <- length(X_full_lst[[2]][,1])
  
  ################# Add-ridge #################
  if (add.ridge){
    for (m in 1:2) {
      weight_full = rep(1, dim(X_full_lst[[m]])[2])
      weight_train = rep(1, dim(X_train_lst[[m]])[2])
      weight_valid = rep(1,dim(X_valid_lst[[m]])[2])
      
      lambda.ridge = add.ridge.lambda.modify(X_train_lst[[m]], Y_train_lst[[m]], 
                                    X_valid_lst[[m]], Y_valid_lst[[m]], 
                                    X_full_lst[[m]], Y_full_lst[[m]], weight_full, weight_train, 
                                    weight_valid)[ridge.tuning]
      
      ridge.results <- add.ridge.fast.modify(X_train_lst[[m]], Y_train_lst[[m]],
                                      X_valid_lst[[m]], Y_valid_lst[[m]],
                                      X_full_lst[[m]], Y_full_lst[[m]], lambda = lambda.ridge, 
                                      weight_full, weight_train, weight_valid)
      Y_train_lst[[m]] <- ridge.results$Y_train
      Y_full_lst[[m]] <- ridge.results$Y_full
    }
  }
  
  if(yes.weight==1){
    weight_full_X_lst = weight.fun.joint(X_full_lst, Y_full_lst)
    weight_train_X_lst = weight.fun.joint(X_train_lst, Y_train_lst)
    weight_valid_X_lst = weight.fun.joint(X_valid_lst, Y_valid_lst)
    
    for (m in 1:2) {
      weight_full_X = weight_full_X_lst[[m]]
      weight_train_X = weight_train_X_lst[[m]]
      weight_valid_X = weight_valid_X_lst[[m]]
      
      X_full_lst[[m]] <- t(t(X_full_lst[[m]]) * weight_full_X^(-2) * sqrt(n_mean / n_vec[m]))
      X_train_lst[[m]] <- t(t(X_train_lst[[m]]) * weight_train_X^(-2) * sqrt(n_mean / n_vec[m]))
      X_valid_lst[[m]] <- t(t(X_valid_lst[[m]]) * weight_valid_X^(-2) * sqrt(n_mean / n_vec[m]))
      
      Y_full_lst[[m]] <- Y_full_lst[[m]] * sqrt(n_mean / n_vec[m])
      Y_train_lst[[m]] <- Y_train_lst[[m]] * sqrt(n_mean / n_vec[m])
      Y_valid_lst[[m]] <- Y_valid_lst[[m]] * sqrt(n_mean / n_vec[m])
      
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
  #pf_group[which(c(1:(p_tot / 2)) %in% common_indx)] <- sqrt(2)
  #pf_group[which(c(1:(p_tot / 2)) %in% common_indx)] <- 1
  ########################## Find scale of lambda ##########################

  lambda_lst <- exp(seq(log(1e-5), log(1e5), length.out = 1000))
  fit_result <- gglasso(x = as.matrix(X_train_all), y = Y_train_all, group = group_ind_vec, 
                        lambda = lambda_lst, pf = pf_group, intercept = F)
  
  min.lambda <- NULL
  min.coef <- NULL
  min.sse <- Inf
  
  XTX <- t(X_valid_all) %*% X_valid_all
  XTY <- t(X_valid_all) %*% Y_valid_all
  
  for (t in 1:length(fit_result$lambda)) {
    #print(t)
    fit_coef <- fit_result$beta[,t]
    sse <- c(t(fit_coef) %*% XTX %*% fit_coef - 2 * t(XTY) %*% fit_coef)
    if (sse < min.sse){
      min.lambda <- fit_result$lambda[t]
      min.sse <- sse
      min.coef <- fit_coef
    }
  }
  
  
  ############ 2nd round Tuning ############
  
  lambda_lst <- seq(min.lambda / 4, min.lambda * 4, length.out = 1000)
  fit_result <- gglasso(x = as.matrix(X_train_all), y = Y_train_all, group = group_ind_vec, 
                        lambda = lambda_lst, pf = pf_group, intercept = F)
  
  min.lambda <- NULL
  min.coef <- NULL
  min.sse <- Inf

  for (t in 1:length(fit_result$lambda)) {
    fit_coef <- fit_result$beta[,t]
    sse <- c(t(fit_coef) %*% XTX %*% fit_coef - 2 * t(XTY) %*% fit_coef)
    
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
