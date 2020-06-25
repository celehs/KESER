# Dropout and up-sample
drop_fun <- function(X, Y, drop_rate = 0.5, up_rate = 2) {
  boot_ind <- sample(1:nrow(X), up_rate * nrow(X), replace = TRUE)
  Y_new <- Y[boot_ind]
  X_new <- X[boot_ind, ]
  drop.gaussian <- MASS::mvrnorm(n = nrow(X_new), mu = rep(0, ncol(X)), Sigma = cor(X))
  drop.ind <- ifelse(drop.gaussian > 0, 1, 0)
  X_new <- drop.ind * X_new + (1 - drop.ind) * (rep(1, nrow(X_new)) %*% t(colMeans(X_new)))
  list(X = X_new, Y = Y_new)
}
