make_past <- function(data, maxlag = 1){
  n <- nrow(data)
  pasts <- lapply(1:maxlag, \(lag) data[lag:(n - maxlag + lag - 1),])
  names(pasts) <- paste0("lag", maxlag:1)
  pasts$deparse.level = 0
  do.call(cbind, pasts)
}


gpca <- function(X, Y, maxlag = 1, 
                 center = TRUE, scale. = TRUE, 
                 tol = NULL, rank. = NULL, intercept = FALSE){

  UseMethod("gpca", X)
}

gpca.prcomp <- function(X, Y, maxlag = 1, 
                        center = TRUE, scale. = TRUE, 
                        tol = NULL, rank. = NULL, intercept = FALSE){
  
  res <- gpca(X$x, Y, maxlag, center, scale., tol, rank., intercept)
  res$rotation <- X$rotation %*% res$rotation
  return(res)
}


gpca.princomp <- function(X, Y, maxlag = 1, 
                        center = TRUE, scale. = TRUE, 
                        tol = NULL, rank. = NULL, intercept = FALSE){
  
  res <- gpca(X$scores, Y, maxlag, center, scale., tol, rank., intercept)
  res$rotation <- X$loadings %*% res$rotation
  return(res)
}

gpca.default <- function(X, Y, maxlag = 1, 
                 center = TRUE, scale. = TRUE, 
                 tol = NULL, rank. = NULL, intercept = FALSE){
  
  if (is.null(dim(Y))) Y <- matrix(data = Y, nrow = length(Y))
  d <- ncol(X)
  X <- scale(X, center = center, scale = scale.)
  Xpast <- make_past(X, maxlag = maxlag)
  if (intercept) Xpast <- cbind(Xpast, 1)
  Ypast <- make_past(Y, maxlag = maxlag)
  XYpast <- cbind(Xpast, Ypast)
  
  if (ncol(XYpast) < nrow(XYpast)){
    qrres <- qr(XYpast)
    Q2 <- qr.Q(qrres)[,(ncol(Xpast) + 1):ncol(XYpast)]    
  }else{
   stop('number of observations in X, minus maxlag, should be greater 
         then number of columns of X times maxlag, try applying PCA and retain 
        just the first components.') 
  }

  X0 <- X[(maxlag+1):nrow(X), ]
  
  X2 <- t(Q2) %*% X0
  n <- nrow(X2)
  p <- ncol(X2)
  k <- if (!is.null(rank.)) {
    stopifnot(length(rank.) == 1, is.finite(rank.), as.integer(rank.) > 
                0)
    min(as.integer(rank.), n, p)
  }else min(n, p)
  
  res <- svd(X2, nu = 0, nv = k)
  rotation <- res$v

  CC <- X %*% rotation
  dimnames(rotation) <- list(colnames(X0), paste0("GPC", seq_len(k)))
  out <- list(x = CC, rotation = rotation)
  
  class(out) <- "gpca"
  return(out)
}

gpca_eigen <- function(X, Y, maxlag = 1, lambda = 0,
                 center = TRUE, scale. = TRUE, intercept = FALSE){
  if (is.null(dim(Y))) Y <- matrix(data = Y, nrow = length(Y))
  d <- ncol(X)
  X <- scale(X, center = center, scale = scale.)
  Xpast <- make_past(X, maxlag = maxlag)
  if (intercept) Xpast <- cbind(Xpast, 1)
  Ypast <- make_past(Y, maxlag = maxlag)
  XYpast <- cbind(Xpast, Ypast)
  
  qrres <- qr(XYpast)
  Q2 <- qr.Q(qrres)[,(ncol(Xpast) + 1):ncol(XYpast)]
  
  X0 <- X[(maxlag+1):nrow(X), ]
  
  EE <- t(X0) %*% ( Q2 %*% t(Q2) * (1 - lambda) +  lambda * diag(1, nrow = nrow(Q2))) %*% X0 

  res <- eigen(EE)
  rotation <- res$vectors
  
  CC <- X %*% rotation
  dimnames(rotation) <- list(colnames(X0), paste0("GPC", seq_len(ncol(X))))
  return(list(x = CC, rotation = rotation))
}

gpca2 <- function(X, Y, maxlag = 1, 
                  center = TRUE, scale. = TRUE, 
                  tol = NULL, rank. = NULL, intercept = FALSE){
  
  if (is.null(dim(Y))) Y <- matrix(data = Y, nrow = length(Y))
  d <- ncol(X)
  X <- scale(X, center = center, scale = scale.)
  Xpast <- make_past(X, maxlag = maxlag)
  if (intercept) Xpast <- cbind(Xpast, 1)
  Ypast <- make_past(Y, maxlag = maxlag)
  XYpast <- cbind(Xpast, Ypast)
  X0 <- X[(maxlag+1):nrow(X), ]
  
  qrres <- qr(XYpast)
  Q2 <- qr.Q(qrres)[,(ncol(Xpast) + 1):ncol(XYpast)] 
  Q3 <- qr.Q(qrres)[,-(ncol(XYpast):ncol(XYpast))]
  
  V <- Q3 %*% t(Q3)
  print(dim(V))
  W <- V + Q2 %*% t(Q2) 
  print(dim(W))
  #A <- t(Q2) %*% X0

  #B <- t(Q3) %*% X0
  
  #res <- geigen::gsvd(A, B)
  
  A <- t(X0) %*% t(W) %*% W %*% X0
  B <- t(X0) %*% t(V) %*% V %*% X0
  res <- geigen::geigen(A, B, symmetric = TRUE)
  
  #lambda <- 0.01
  #res <- eigen(solve(B + lambda*diag(ncol(B))) %*% A, symmetric = TRUE)
    
  rotation <- res$vectors[,ncol(res$vectors):1]
  CC <- X %*% rotation
  dimnames(rotation) <- list(colnames(X0), paste0("GPC", seq_len(ncol(rotation))))
  return(list(x = CC, rotation = rotation))
}


tlopls <- function(X, Y, maxlag = 1, rank = 1, scaleC = 'center'){
  if (is.null(dim(Y))) Y <- matrix(data = Y, nrow = length(Y))
  d <- ncol(X)
  Ypast <- make_past(Y, maxlag = maxlag)
  X0 <- X[(maxlag+1):nrow(X), ]
  
  res <- ropls::opls(X0, Ypast, predI = rank, scaleC = scaleC)
  rotation <-  res@weightStarMN
  CC <- X %*% rotation
  return(list(x = CC, rotation = rotation))
}


tlcancor <- function(X, Y, maxlag = 1, scale = TRUE, center = TRUE){
  X <- scale(X, scale = scale, center = center)
  if (is.null(dim(Y))) Y <- matrix(data = Y, nrow = length(Y))
  d <- ncol(X)
  Ypast <- make_past(Y, maxlag = maxlag)
  X0 <- X[(maxlag+1):nrow(X), ]
  
  res <- cancor(X0, Ypast)
  print(res)
  rotation <- res$xcoef
  print(dim(rotation))
  print(dim(X))
  CC <- X %*% rotation
  return(list(x = CC, rotation = rotation))
}



make_random_features <- function(x, n = 100, sd = 1){
  b <- 2*pi*runif(1)
  w <- matrix(rnorm(n * ncol(x), mean = 0, sd = sd), ncol = n)
  z <- cos(x %*% w + b)
  return(z)
}


# C must include B to be a granger test
granger_test <- function(A, B, C, p){
  x <- B[(p+1):nrow(A)]
  y <- make_past(A, maxlag = p)
  z <- make_past(C, maxlag = p)
  model1 <- lm(x ~ y + z)
  model0 <- lm(x ~ z)
  #lmtest::waldtest(model1, model0)
  anova(model0, model1, test = "F")
}
