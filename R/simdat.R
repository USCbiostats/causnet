#' Simulate example data
#'
#' @param sample.size integer, number of observations
#' @param n.var integer, number of variables
#' @param beta numeric, amount of correlation
#'
#' The dependencies will be created in the fashion: 1 -> 2 -> 3 -> 4 -> 5
#'
#' @importFrom stats rnorm
#'
#' @return data.frame
#' @export

#' Simulate example data
#'
#' @param sample.size integer, number of observations
#' @param n_var integer, number of variables
#' @param beta numeric, amount of correlation
#'
#' The dependencies will be created in the fashion: 1 -> 2 -> 3 -> 4 -> 5
#'
#' @importFrom stats rnorm
#'
#' @return data.frame
#' @export
simdat <- function(sample.size = 300, n_var = 5, beta = 0.3) {
  myerror <- matrix(rnorm(sample.size * n_var), ncol = n_var)
  mydata <- matrix(rnorm(sample.size * n_var), ncol = n_var)


  for (j in seq_len(n_var - 1)) {
    mydata[, j + 1] <- beta * mydata[, j] + myerror[, j + 1]
  }
  mydata <- as.data.frame(scale(mydata))
  return(mydata)
} # end simdat

