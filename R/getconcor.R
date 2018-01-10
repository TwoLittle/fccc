#' @title Compute the conventional Pearson's correlation and concordance correlation
#'        of functional data
#'
#' @description For each subject/row, we first obtain the average of all time points
#'              and then compute the conventional Pearson's correlation
#'              coefficient and concordance correlation coefficeint
#'
#' @param X: matrix with columns being time points and rows being subjects
#' @param Y: matrix with columns being time points and rows being subjects, same size as X
#'
#' @return rho: conventional Pearson's correlation coefficient
#' @return ccc: conventional concordance correlation coefficient
#'
#' @export get.con.cor
#'
#' @examples
#' x = matrix(rnorm(12), 3, 4)
#' y = matrix(rnorm(12), 3, 4)
#' get.con.cor(x,y)

get.con.cor <- function(X, Y){

  x <- rowMeans(X, na.rm = T)
  y <- rowMeans(Y, na.rm = T)

  idx <- !(is.nan(x) | is.nan(y))
  nx <- x[idx]
  ny <- y[idx]
  mux <- mean(nx)
  muy <- mean(ny)
  ccc <- 2*cov(nx, ny) / ((mux - muy)^2 + var(nx) + var(ny))
  rho <- cor(nx,ny)

  return(list(rho = rho, ccc = ccc))

}
