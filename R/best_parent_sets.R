#' Get BEST parent sets and scores for all sets of possible parents for each node
#'
#' @param ms Numeric vector, node scores for no parents. Output of
#'     no_parent_score. Should have same number of elements as variables in
#'     data.
#' @param pps Possible parent sets, output from `find_possible_parent_sets`.
#' @param ppss Possible parent set scores, output from
#'     `score_possible_parent_sets`.
#'
#' @return list with 2 list. First list is list of lists with best parent set.
#'     Second list is a list of lists with scores of the best network.
#' @noRd
best_possible_parent_sets <- function(ms, pps, ppss) {
  # for each possible parent set, compare score w/ all subsets to identify the
  # best possible parent sets
  tmp <- lapply(seq_along(pps), function(y) lapply(pps[[y]], function(x) bestps(y, x, pps, ppss, ms)))

  out <- list()
  out[[1]] <- lapply(seq_along(tmp), function(y) lapply(tmp[[y]], function(x) x[[1]]))
  out[[2]] <- lapply(seq_along(tmp), function(y) lapply(tmp[[y]], function(x) x[[2]]))
  out
}

# function to find best parent set of s, for parent set pset, given pps, ppss,
# ms
bestps <- function(v, pset, pps, ppss, ms) {
  # all subsets of pset
  best_score <- get.score(v, NULL, pps, ppss, ms)
  best_set <- NULL
  sb <- comb1(pset)
  for (j in seq_along(sb)) {
    for (k in seq_len(ncol(sb[[j]]))) {
      tmp_set <- sb[[j]][, k]
      tmp_score <- get.score(v, tmp_set, pps, ppss, ms)
      if (tmp_score > best_score) {
        best_set <- tmp_set
        best_score <- tmp_score
      }
    }
  }
  outp <- vector("list", 2)
  outp[[1]] <- best_set
  outp[[2]] <- best_score
  outp
}

# get a score, given a node, its parent set, and parent set scores
# ms = scores of nodes w/ no parents
get.score <- function(v, pset, pps, ppss, ms) {
  if (length(pset) < 1) {
    myscore <- ms[v]
  } else {
    l <- length(pset)
    aa <- vapply(pps[[v]], function(x) setequal(x, pset), FUN.VALUE = logical(1))
    myscore <- ppss[[v]][aa]
  }
  return(myscore)
} # end get.score


