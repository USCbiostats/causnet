#' Create all sets of possible parents for each node
#'
#' @details
#' Each vector of possible parents will be turned into a list of matrices of
#' possible parent sets.
#'
#' @param possible_parents A list of integers, output from
#'     find_possible_parents.
#' @param max_parents Numeric, maximal number of parents.
#'
#' @return list of lists of matrices.
#'
#' @noRd
find_possible_parent_sets <- function(possible_parents, max_parents) {
  pps_mat2list(lapply(possible_parents, comb1, max_parents = max_parents))
}

#' Calculate all possible parent set of a set
#'
#' @noRd
comb1 <- function(vec, max_parents) {
  lapply(seq_len(min(length(vec), max_parents)), combn_vec, x = vec)
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

ps2list <- function(ps) {
  Reduce(c, lapply(ps, mat2list))
}

mat2list <- function(mat) {
  lapply(seq_len(ncol(mat)), function(x) mat[, x])
}

pps_mat2list <- function(pps) {
  lapply(pps, ps2list)
}
