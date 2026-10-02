#' get edges of best network connected components from ordered sinks
#'
#' @param bnets  a data.frame with 4 variables, wscore, windx, k, sink. output
#'     from `best_network`.
#' @param possible_parents A list of integers, output from
#'     find_possible_parents.
#' @param pps Possible parent sets, output from `find_possible_parent_sets`.
#' @param bps A list of best possible parent set, output from
#'     `best_possible_parent_sets`.
#' @param max_parents Numeric, maximal number of parents.
#'
#' @return List with two elements. First element is network data.frame with 3
#'     elements; node.source, node.sink and component, with one row for every
#'     parent in the best parent set of every sink. And vector n_best_parents
#'     indicating number of best parents at each consideration.
#' @noRd
sink2net <- function(bnets, pp, pps, bps, max_parents) {

  # TODO change wscore to component in name only
  mynets <- data.frame(node.source = numeric(),
                       node.sink = numeric(),
                       component = numeric())
  n_comp <- length(unique(bnets$wscore)) # component
  rowno <- 1
  n_conflicts <- integer()

  for (c_index in seq_len(n_comp)) {
    indexes <- bnets$wscore == c_index
    for (k in seq_down_to_2(max(bnets$k[indexes]))) {
      bp_set <- NULL
      tmp <- which(indexes)[bnets$k[indexes] == k]
      if(length(tmp) == 0) break

      s <- bnets$sink[tmp][[1]]
      w <- bnets$windx[tmp][[1]]

      if (is.null(w)) next
      bp_set <- swscore(s, w, pp, pps, bps, max_parents)[[1]]

      if (is.null(bp_set)) next
      if (any(is.infinite(bp_set))) next

      n_conflicts <- c(n_conflicts, length(bp_set))
      # every parent in the best parent set of sink s becomes an edge
      new_rows <- rowno + seq_along(bp_set) - 1
      mynets[new_rows, ] <- cbind(bp_set, s, c_index)
      rowno <- rowno + length(bp_set)
    }
  }

  list(network = mynets,
       n_best_parents = n_conflicts)
}
