#' @title Main function for integrative feature selection
#' @param X_full_lst ...
#' @param Y_full_lst ...
#' @param X_train_lst ...
#' @param Y_train_lst ...
#' @param X_valid_lst ...
#' @param Y_valid_lst ...
#' @param lambda_lst ...
#' @param add.ridge ...
#' @export
int.feature.selection.fast <- function(
  X_full_lst, Y_full_lst, X_train_lst, Y_train_lst, X_valid_lst, Y_valid_lst,
  lambda_lst = NULL, add.ridge = TRUE) {
  ################# Drop-out #################
  n1_valid <- length(X_valid_lst[[1]][, 1])
  n2_valid <- length(X_valid_lst[[2]][, 1])
  for (m in 1:2) {
    drop_m_full <- drop_fun(X_full_lst[[m]], Y_full_lst[[m]], drop_rate = 0.5, up_rate = 10)
    drop_m_train <- drop_fun(X_train_lst[[m]], Y_train_lst[[m]], drop_rate = 0.5, up_rate = 10)
    X_full_lst[[m]] <- drop_m_full$X
    Y_full_lst[[m]] <- drop_m_full$Y
    X_train_lst[[m]] <- drop_m_train$X
    Y_train_lst[[m]] <- drop_m_train$Y
  }
  p1 <- length(X_full_lst[[1]][1, ])
  p2 <- length(X_full_lst[[2]][1, ])
  n1 <- length(X_full_lst[[1]][, 1])
  n2 <- length(X_full_lst[[2]][, 1])
  ################# Add-ridge #################
  if (add.ridge) {
    for (m in 1:2) {
      ridge.results <- add.ridge.fast(
        X_train_lst[[m]], Y_train_lst[[m]],
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
    if (m == 1) {
      X_full_all[1:n1, 2 * which(!is.na(match_m)) - 1] <- X_full_lst[[m]][, match_m[which(!is.na(match_m))]]
      X_train_all[1:n1, 2 * which(!is.na(match_m)) - 1] <- X_train_lst[[m]][, match_m[which(!is.na(match_m))]]
      X_valid_all[1:n1_valid, 2 * which(!is.na(match_m)) - 1] <- X_valid_lst[[m]][, match_m[which(!is.na(match_m))]]
    }
    if (m == 2) {
      X_full_all[n1 + 1:n2, 2 * which(!is.na(match_m))] <- X_full_lst[[m]][, match_m[which(!is.na(match_m))]]
      X_train_all[n1 + 1:n2, 2 * which(!is.na(match_m))] <- X_train_lst[[m]][, match_m[which(!is.na(match_m))]]
      X_valid_all[n1_valid + 1:n2_valid, 2 * which(!is.na(match_m))] <- X_valid_lst[[m]][, match_m[which(!is.na(match_m))]]
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
  if (is.null(lambda_lst)) {
    fit.cv <- gglasso::cv.gglasso(
      x = as.matrix(X_train_all), y = Y_train_all, group = group_ind_vec, 
      pf = pf_group, intercept = FALSE)
    lambda.min <- fit.cv$lambda.min
    lambda_lst = c(1e-3 * (1:999), 1:1000) * lambda.min
  }
  ############################################################################
  fit_result <- gglasso::gglasso(
    x = as.matrix(X_train_all), y = Y_train_all, group = group_ind_vec, 
    lambda = lambda_lst, pf = pf_group, intercept = FALSE)
  min.lambda <- NULL
  min.coef <- NULL
  min.sse <- Inf
  XTX <- t(X_valid_all) %*% X_valid_all
  XTY <- t(X_valid_all) %*% Y_valid_all
  for (t in 1:length(fit_result$lambda)) {
    fit_coef <- fit_result$beta[, t]
    sse <- c(t(fit_coef) %*% XTX %*% fit_coef - 2 * t(XTY) %*% fit_coef)
    # print(sse)
    # print(length(which(fit_coef != 0)))
    if (sse < min.sse) {
      min.lambda <- fit_result$lambda[t]
      min.sse <- sse
      min.coef <- fit_coef
    }
  }
  ############# bind back to form beta according to the original index #############
  fit_result <- gglasso::gglasso(
    x = as.matrix(X_full_all), y = Y_full_all, group = group_ind_vec, 
    lambda = min.lambda, pf = pf_group, intercept = FALSE)
  fit_coef <- fit_result$beta
  beta_lst <- vector('list', 2)
  for (m in 1:2) {
    match_m <- match(var_set_all, colnames(X_full_lst[[m]]))
    beta_lst[[m]] <- rep(0, ncol(X_full_lst[[m]]))
    beta_lst[[m]][match_m[which(!is.na(match_m))]] <- fit_coef[2 * which(!is.na(match_m)) + m - 2]
    beta_lst[[m]] <- as.data.frame(beta_lst[[m]])
    rownames(beta_lst[[m]]) <- colnames(X_full_lst[[m]])
  }
  beta.tab <- merge(beta_lst[[1]], beta_lst[[2]], by = 'row.names', all = TRUE)
  rownames(beta.tab) <- beta.tab$Row.names
  beta.tab$Row.names <- NULL
  colnames(beta.tab) <- c('Study_1', 'Study_2')
  list(lambda = min.lambda, coefficients = beta.tab)
}
