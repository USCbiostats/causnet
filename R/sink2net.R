# get edges of best network connected components from ordered sinks
sink2net <- function(bnets, pp, pps, bps) {
  m <- length(bps[[1]])
  nms <- c("node.source", "node.sink", "component")
  mynets <- as.data.frame(matrix(NA, nrow = 0, ncol = length(nms)))
  names(mynets) <- nms
  n_comp <- length(unique(bnets$component))
  rowno <- 1
  n_conflicts <- integer()

  for (c_index in seq_len(n_comp)) {
    tmpn <- bnets[is.element(bnets$component, c_index), ]
    for (k in max(tmpn$k):2) {
      bp_set <- NULL
      tmp <- tmpn[is.element(tmpn$k, k), ]
      s <- tmp[1, "sink"]
      w <- subsetur(m, tmp[1, "windx"])
      bp_set <- swscore(s, w, pp, pps, bps)[[1]]
      if (!is.null(bp_set[[1]])) {
        src <- bp_set
        n_conflicts <- c(n_conflicts, length(src[[1]]))
        # Sampling 1 best sink at random
        src[[1]] <- src[[1]][sample.int(length(src[[1]]), 1)]

        snk <- rep(s, length(bp_set))
        cmp <- rep(c_index, length(bp_set))
        mynets[rowno:(rowno + length(bp_set) - 1), ] <- cbind(src, snk, cmp)
        rowno <- rowno + length(bp_set)
      }
    }
  }
  return(list(network = mynets,
              n_best_parents = n_conflicts))
} # end sink2net

#' get edges of best network connected components from ordered sinks
#'
#' @param bnets  a data.frame with 4 variables, wscore, windx, k, sink. output
#'     from `best_network`.
#' @param possible_parents A list of integers, output from
#'     find_possible_parents.
#' @param pps Possible parent sets, output from `find_possible_parent_sets`.
#' @param bps A list of best possible parent set, output from
#'     `best_possible_parent_sets`.
#'
#' @return List with two elements. First element is network data.frame with 3
#'     elements; node.source, node.sink and component. And vector n_best_parents
#'     indicating number of best parents at each consideration.
#' @noRd
sink2net1 <- function(bnets, pp, pps, bps) {
  m <- length(bps[[1]])
  nms <- c("node.source", "node.sink", "component")
  mynets <- as.data.frame(matrix(NA, nrow = 0, ncol = length(nms)))
  names(mynets) <- nms
  n_comp <- length(unique(bnets$component))
  rowno <- 1
  n_conflicts <- integer()

  for (c_index in seq_len(n_comp)) {
    tmpn <- bnets[is.element(bnets$component, c_index), ]
    for (k in max(tmpn$k):2) {
      bp_set <- NULL
      tmp <- tmpn[is.element(tmpn$k, k), ]
      s <- tmp[1, "sink"]
      w <- subsetur(m, tmp[1, "windx"])
      bp_set <- swscore1(s, w, pp, pps, bps)[[1]]
      if (!is.null(bp_set[[1]])) {
        src <- bp_set
        n_conflicts <- c(n_conflicts, length(src[[1]]))
        # Sampling 1 best sink at random
        src[[1]] <- src[[1]][sample.int(length(src[[1]]), 1)]

        snk <- rep(s, length(bp_set))
        cmp <- rep(c_index, length(bp_set))
        mynets[rowno:(rowno + length(bp_set) - 1), ] <- cbind(src, snk, cmp)
        rowno <- rowno + length(bp_set)
      }
    }
  }
  list(network = mynets,
       n_best_parents = n_conflicts)
}
