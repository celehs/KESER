cosine.fun <- function(x, y) {
  crossprod(x, y) / sqrt(crossprod(x) * crossprod(y))
}

lasso.fun <- function(x.A, y.A, x.sam, y.sam, x.sam2, y.sam2, cos.sims, nlambda, alpha, seed = 1234) {
  set.seed(seed)
  obj1 <- glmnet::cv.glmnet(
    x = x.A, y = y.A, alpha = alpha, 
    intercept = FALSE, standardize = FALSE,
    penalty.factor = 1 / abs(cos.sims))
  max.lambda <- obj1$lambda.min * 1e2 
  min.lambda <- obj1$lambda.min * 1e-6
  grid.lambda <- seq(from = max.lambda, 
                     to = min.lambda, 
                     length.out = nlambda)
  obj.A <- glmnet::glmnet(
    x = x.A, y = y.A,
    alpha = alpha, lambda = grid.lambda, 
    intercept = FALSE, standardize = FALSE,
    penalty.factor = 1 / abs(cos.sims))
  obj.sam <- glmnet::glmnet(
    x = x.sam, y = y.sam,
    alpha = alpha, lambda = grid.lambda,
    intercept = FALSE, standardize = FALSE,
    penalty.factor = 1 / abs(cos.sims))
  B.sam <- as.matrix(glmnet::coef.glmnet(obj.sam))[-1, ]
  B.A <- as.matrix(glmnet::coef.glmnet(obj.A))[-1, ]
  MSE <- unlist(lapply(1:nlambda, function(ll) {
    mean((y.sam2 - x.sam2 %*% matrix(B.sam[, ll], ncol = 1))^2)
  }))
  B.lasso <- as.matrix(B.A[, which.min(MSE)])
  B.lasso <- data.frame(rownames(B.lasso), cos.sims, B.lasso)
  colnames(B.lasso) <- c("variable", "cos.sims", "coefficient")
  rownames(B.lasso) <- NULL
  B.lasso[order(-B.lasso[, "cos.sims"]), ]
}

drop.dat.fun <- function(x, y, drop.rate, up.rate = 10) {
  tmpind <- sample(1:nrow(x), up.rate * nrow(x), replace = TRUE)
  ynew <- y[tmpind]
  xnew <- x[tmpind, ] 
  colnames(xnew) <- colnames(x)
  col.drop <- 1:ncol(xnew)
  xnew[,col.drop] <- sapply(col.drop, function(kk) {
    tmpx <- xnew[, kk] 
    tmpx[sample(1:length(tmpx), round(drop.rate * length(tmpx)))] <- mean(tmpx) 
    tmpx
  })
  list(ynew = ynew, xnew = xnew)
}

dropout.fun <- function(x.A, y.A, x.sam, y.sam, x.sam2, y.sam2, 
                        cos.sims, nlambda, alpha, up.rate, drop.rate, seed = 1234) {
  set.seed(seed)
  junk1 <- drop.dat.fun(x.A, y.A, drop.rate, up.rate)
  xnew.A <- junk1$xnew
  ynew.A <- junk1$ynew
  cv.fit1 <- glmnet::cv.glmnet(
    xnew.A, ynew.A, 
    family = "gaussian",
    intercept = FALSE, 
    alpha = alpha, 
    standardize = FALSE,
    penalty.factor = 1 / abs(cos.sims))
  max.lambda <- cv.fit1$lambda.min * 1e2 
  min.lambda <- cv.fit1$lambda.min * 1e-6
  grid.lambda <- seq(
    from = max.lambda, 
    to = min.lambda, 
    length.out = nlambda)
  ## step 2: training
  set.seed(seed)
  junk2.sam <- drop.dat.fun(x.sam, y.sam, drop.rate, up.rate)
  xnew.sam <- junk2.sam$xnew
  ynew.sam <- junk2.sam$ynew
  fit.sam <- glmnet::glmnet(
    xnew.sam, ynew.sam, 
    family = "gaussian",
    intercept = FALSE,
    alpha = alpha,
    standardize = FALSE,
    penalty.factor = 1 / abs(cos.sims), 
    lambda = grid.lambda)
  junk2.A <- drop.dat.fun(x.A, y.A, drop.rate, up.rate)
  xnew.A <- junk2.A$xnew
  ynew.A <- junk2.A$ynew
  fit.A <- glmnet::glmnet(
    xnew.A, ynew.A,
    family = "gaussian",
    intercept = FALSE,
    alpha = alpha,
    standardize = FALSE,
    penalty.factor = 1 / abs(cos.sims), 
    lambda = grid.lambda)
  B.sam <- as.matrix(glmnet::coef.glmnet(fit.sam))[-1, ]
  B.A <- as.matrix(glmnet::coef.glmnet(fit.A))[-1, ]
  ## step 3: validation
  MSE <- unlist(lapply(1:nlambda, function(ll) { 
    mean((y.sam2 - x.sam2 %*% matrix(B.sam[, ll], ncol = 1))^2) 
  }))
  B.dropout <- as.matrix(B.A[, which.min(MSE)])
  B.dropout <- data.frame(rownames(B.dropout), cos.sims, B.dropout)
  colnames(B.dropout) <- c("variable", "cos.sims", "coefficient")
  rownames(B.dropout) <- NULL
  B.dropout[order(-B.dropout[, "cos.sims"]), ]
}

#' @title ...
#' @param x.A ...
#' @param y.A ...
#' @param x.sam ...
#' @param y.sam ...
#' @param x.sam2 ...
#' @param y.sam2 ...
#' @param method ...
#' @param alpha ...
#' @param nlambda ...
#' @param up.rate ...
#' @param drop.rate ...
#' @param cos.cut ...
#' @export
feature.sel.fun <- function(x.A, y.A, x.sam, y.sam, x.sam2, y.sam2, method, alpha, 
                            nlambda = 500, up.rate = 10, drop.rate = 0.5, cos.cut) {
  cos.sims <- unlist(lapply(1:ncol(x.A), function(j) cosine.fun(y.A, x.A[, j])))
  exclude.indx <- which(cos.sims < cos.cut)
  if (method == "lasso") {
    res <- lasso.fun(
      x.A[, -exclude.indx], y.A, 
      x.sam[, -exclude.indx], y.sam, 
      x.sam2[,-exclude.indx], y.sam2, 
      cos.sims[-exclude.indx], nlambda, alpha)
  } else if (method == "dropout.without.y") {
    res <- dropout.fun(
      x.A[,-exclude.indx], y.A, 
      x.sam[, -exclude.indx], y.sam, 
      x.sam2[,-exclude.indx], y.sam2, 
      cos.sims[-exclude.indx], nlambda, alpha, 
      up.rate, drop.rate)
  } else if (method == "dropout.with.y") {
    x.A.new <- cbind(y = y.A, x.A[, -exclude.indx])
    x.sam.new <- cbind(y = y.sam, x.sam[, -exclude.indx])
    x.sam2.new <- cbind(y = y.sam2, x.sam2[, -exclude.indx])
    cos.sims.new <- c(1,cos.sims[-exclude.indx])
    res <- dropout.fun(
      x.A.new, y.A, 
      x.sam.new, y.sam, 
      x.sam2.new, y.sam2, 
      cos.sims.new, nlambda, alpha, 
      up.rate, drop.rate)
  }
  return(res)
}
