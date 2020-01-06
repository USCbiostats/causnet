#' Function to find possible offspring
#'
#' @details
#' This function takes a list of possible parents and calculates the
#' corresponding list of possible offpring.
#'
#' @param possible_parents A list of integers, output from
#'     find_possible_parents.
#'
#' @return A list of integers corresponding to the indices.
#'
#' @noRd
find_possible_offspring <- function(possible_parents) {
  out <- vector("list", length(possible_parents))
  for (i in seq_along(possible_parents)) {
    for (j in seq_along(possible_parents)) {
      if (is.element(i, possible_parents[[j]])) {
        out[[i]] <- c(out[[i]], j)
      }
    }
  }
  out
}
