#' Create all sets of possible parents for each node
#'
#' @details
#' Each vector of possible parents will be turned into a list of matrices of
#' possible parent sets.
#'
#' @param possible_parents A list of integers, output from
#'     find_possible_parents.
#'
#' @return list of lists of matrices.
#'
#' @noRd
find_possible_parent_sets <- function(possible_parents) {
  lapply(possible_parents, comb1)
}

#' Calculate all possible parent set of a set
#'
#' @noRd
comb1 <- function(vec) {
  lapply(seq_along(vec), combn_vec, x = vec)
}

#' Makes combn work as if the input is always a vector
#'
#' @noRd
combn_vec <- function(n, x) {
  if (length(x) == 1) {
    return(matrix(x, nrow = 1, ncol = 1))
  } else {
    return(utils::combn(x, n))
  }
}
