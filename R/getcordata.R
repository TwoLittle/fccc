#' @title Generate correlated data
#'
#' @description Given a data set X in matrix form, generate a new data set sim.data such that
#'              the functional Pearson's correlation between X and sim.data is rho. Also
#'              return the corresponding functional concordance correlation coefficient
#'
#' @param X: matrix with columns being time points and rows being subjects
#' @param rho: functional Pearson's correlation coefficient
#' @param weights: a vector of weights whose length is number of time points
#' @param upper: the maximum value in the generated data set
#' @param round: If round is True, the generated data set will be rounded to the nearest integer.
#'               By default, round equals True
#'
#' @return sim.data: correlated data set
#' @return rhoc: functional concordance correlation coefficient between X and sim.data
#'
#' @export get.cor.data
#'
#' @examples
#' x = matrix(rnorm(12), 3, 4)
#' y = matrix(rnorm(12), 3, 4)
#' w = c(1,1,1,1)
#' get.cor.data(x, rho = 0.5, weights = w)

get.cor.data <- function(X, rho, weights, upper = 10000, round = T){

  # generate a new vecotr that is correlated with a given vector with correlation r
  r.empr <- function(x, r){
    # r is correlation

    n <- length(x)
    DT <- data.table(x)
    DT <- DT[, .(p = .N/n), by = "x"]

    if(length(DT$x) == 1){
      x1 <- rep(DT$x, n)
    } else {
      x1 <- sample(x = DT$x, size = n, prob = DT$p, replace = T)
    }

    y <- r*x + sqrt(1-r^2)*x1

    return(y)

  }

  # Get the emprical distribution for a given data
  get.dist <- function(data){
    # data is a vector

    x <- data[!is.nan(data)]
    n <- length(x)
    x.dt <- data.table(x)
    x.dt <- x.dt[, .(p = .N/n), by = "x"]
    x.dt <- x.dt[order(x)]

    return(x.dt)

  }


  data <- as.matrix(X)
  nsubj <- nrow(data)
  ndays <- ncol(data)

  sim.data <- matrix(NaN, nrow = nsubj, ncol = ndays)
  vrhoc <- rep(NA, ndays)
  weights <- weights[1:ndays]

  for(t in 1:ndays){

    idx <- !is.nan(data[,t])
    if(sum(idx) == 0) next
    x <- data[idx, t]
    mu_x <- mean(x)
    sigma2_x <- var(x)
    vrhoc[t] <- 1/( 1/rho + (rho + sqrt(1-rho^2) - 1)^2*mu_x^2/(2*rho*sigma2_x) )

    y <- r.empr(x, rho)
    if(round) {
      y = round(y)
    }
    y[y > upper] <- upper
    sim.data[idx, t] <- y

  }

  index <- !is.na(vrhoc)
  rhoc <- sum(vrhoc[index] * weights[index]) / sum(weights[index])

  return(list(sim.data = sim.data, rhoc = rhoc))

}

