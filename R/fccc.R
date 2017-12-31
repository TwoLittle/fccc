# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

myhello <- function() {
  print("Hello, world!")
}

# Find the "covariance" between data set X and Y with weight matrix W
mCov <- function(X, Y, W){

  nt    <- ncol(X)
  nsubj <- nrow(X)
  XW    <- X*W
  YW    <- Y*W
  XW[is.nan(XW)] <- 0
  YW[is.nan(YW)] <- 0

  Xsum  <- rep(NA, nsubj)
  Ysum  <- rep(NA, nsubj)

  for(i in 1:nsubj){
    Xsum[i] <- sum(XW[i,W[i,]!=0])
    Ysum[i] <- sum(YW[i,W[i,]!=0])
  }

  mean(Xsum*Ysum) - mean(Xsum)*mean(Ysum)

}

### Get the functional rho/ccc and their standard error
get.fun.cor <- function(X, Y, W=NULL){
  # X: data from measurement 1
  # Y: data from measurement 2
  # W: weight for all data points
  # X,Y,W: are all matrix of the same size

  nt    <- ncol(X)
  nsubj <- nrow(X)
  Xbar  <- rep(NA, nt)
  Ybar  <- rep(NA, nt)

  if(is.null(W)){

    W <- matrix(1, nsubj, nt)
    W[is.nan(X) | is.nan(Y)] <- 0

  }

  W <- W[1:nsubj, 1:nt]
  W[is.nan(X) | is.nan(Y)] <- 0

  for(j in 1:nt){
    Xbar[j] <- mean(X[W[,j]!=0,j])
    Ybar[j] <- mean(Y[W[,j]!=0,j])
  }

  mXbar <- matrix(Xbar, nsubj, nt, byrow = T)
  mYbar <- matrix(Ybar, nsubj, nt, byrow = T)

  X.center <- X - mXbar
  Y.center <- Y - mYbar
  inn.XY.center <- sum((X.center*Y.center*W)[W!=0])
  inn.XX.center <- sum((X.center*X.center*W)[W!=0])
  inn.YY.center <- sum((Y.center*Y.center*W)[W!=0])
  inn.df.xybar  <- sum(((mXbar - mYbar)^2*W)[W!=0])
  rho <- inn.XY.center/sqrt(inn.XX.center*inn.YY.center)
  ccc <- 2*inn.XY.center/(inn.df.xybar + inn.XX.center + inn.YY.center)

  term1 <- X.center*Y.center
  term2 <- X*X
  term3 <- Y*Y
  term4 <- (X.center*mYbar + Y*mXbar)
  term5 <- X.center*X.center
  term6 <- Y.center*Y.center
  term.ccc <- array(c(term1, term2, term3, term4), dim = c(dim(term1), 4))
  term.rho <- array(c(term1, term5, term6), dim = c(dim(term1), 3))

  Sigma.ccc <- matrix(NA, 4, 4)
  Sigma.rho <- matrix(NA, 3, 3)

  for(i in 1:4){
    for(j in 1:4){
      Sigma.ccc[i,j] <- mCov(term.ccc[,,i], term.ccc[,,j], W)
    }
  }

  for(i in 1:3){
    for(j in 1:3){
      Sigma.rho[i,j] <- mCov(term.rho[,,i], term.rho[,,j], W)
    }
  }

  A <- (X^2 + Y^2 - 2*X*Y)*W
  A[is.nan(A)] <- 0
  a.ccc <- c(2, -ccc, -ccc, 2*ccc)/mean(apply(A, 1, sum))
  ma.ccc1 <- matrix(a.ccc, 4, 4, byrow = F)
  ma.ccc2 <- matrix(a.ccc, 4, 4, byrow = T)
  se.ccc  <- sqrt(sum(ma.ccc1*Sigma.ccc*ma.ccc2))/sqrt(nsubj-2)

  B2 <- inn.XX.center/nsubj
  B3 <- inn.YY.center/nsubj
  B1 <- sqrt(B2*B3)
  a.rho <- c(1/B1, -rho/(2*B2), -rho/(2*B3))
  ma.rho1 <- matrix(a.rho, 3, 3, byrow = F)
  ma.rho2 <- matrix(a.rho, 3, 3, byrow = T)
  se.rho  <- sqrt(sum(ma.rho1*Sigma.rho*ma.rho2))/sqrt(nsubj-2)

  return(list(rho=rho, ccc=ccc, se.rho=se.rho, se.ccc=se.ccc))
  # return the functional rho and ccc and their standard errors

}

get.con.cor <- function(X, Y){

  x <- rowMeans(X, na.rm = T)
  y <- rowMeans(Y, na.rm = T)

  idx <- !(is.nan(x) | is.nan(y))
  nx <- x[idx]
  ny <- y[idx]
  mux <- mean(nx)
  muy <- mean(ny)
  ccc <- 2*cov(nx, ny) / ((mux - muy)^2 + var(nx) + var(ny))
  rho <- cor(x,y)

  return(list(rho = rho, ccc = ccc))

}
