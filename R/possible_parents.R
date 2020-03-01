#' Function to find possible parents
#'
#' @details
#' This function takes the data.frame and a p-value threshold alpha.
#' Then it iteratively looks at each node (varible) and located possible parents
#' determined by the pearson correlation between the nodes.
#' The result is a list of interger vectors. The the first vector in the list
#' represent the index of the possible parents to the first node.
#' If `c(2, 6, 8)` is element number 1, then node 2, 6 and 8 are possible
#' parents to node 1.
#'
#' @param data A data.frame of numeric variables.
#' @param alpha Numerical, p-value threshold to determine cutoff for possible
#'   parents.
#'
#' @return A list of integers corresponding to the indices.
#'
#' @noRd
find_possible_parents <- function(data, alpha) {
  n_var <- ncol(data)

  out <- vector("list", n_var)
  for (vertex in seq_len(n_var)) {
    for (parent in seq_len(n_var)) {
      if (parent != vertex) {
        p_value <- stats::cor.test(data[, vertex, drop = TRUE],
                                   data[, parent, drop = TRUE])$p.value
        if (p_value < alpha) out[[vertex]] <- c(out[[vertex]], parent)
      }
    }
  }
  out
}
