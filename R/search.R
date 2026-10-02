#' Search the best network given candidate parents
#'
#' @details
#' Shared by `causnet()` and `causnet_stable()`: optional phenotype-driven
#' restriction (`pheno_expand()`), feasSet reduction (`feas_reduce()`), the
#' dynamic programme on the reduced problem (`causnet_dp()`), and translation
#' of the node indices back to the columns of `data`.
#'
#' @param data A data.frame of numeric variables.
#' @param possible_parents A list of integers, one element per column of
#'     `data`, with `NULL` for nodes without candidate parents.
#' @param max_parents Numeric, maximal number of parents.
#' @param score_fun A function used for scoring parent sets.
#' @param outcome `NULL`, or integer index of the outcome column.
#' @param n_levels Integer, number of hops of the phenotype-driven search.
#'
#' @return An object of class `causnet`.
#' @noRd
causnet_search <- function(data, possible_parents, max_parents, score_fun,
                           outcome = NULL, n_levels = 3) {
  if (!is.null(outcome)) {
    possible_parents <- pheno_expand(possible_parents, outcome, n_levels)
  }

  fr <- feas_reduce(possible_parents)

  if (length(fr$feas) == 0) {
    out <- list(network = data.frame(node.source = numeric(),
                                     node.sink = numeric(),
                                     component = numeric()),
                n_best_parents = integer(),
                mutliple_networks = NULL)
  } else {
    out <- causnet_dp(data[, fr$feas, drop = FALSE], fr$possible_parents,
                      max_parents, score_fun)
    # node indices of the reduced problem -> columns of `data` (kept as
    # doubles, the type sink2net() returns)
    out$network$node.source <- as.numeric(fr$feas[out$network$node.source])
    out$network$node.sink <- as.numeric(fr$feas[out$network$node.sink])
    out$mutliple_networks$feas <- fr$feas
  }

  names(out[[1]])[1:2] <- c("from", "to")
  class(out) <- "causnet"
  out
}

#' Dynamic programme and edge extraction
#'
#' @details
#' Runs the dynamic programme on `data` with candidate parents
#' `possible_parents` (node indices refer to the columns of `data`) and
#' returns the edges of the best network.
#'
#' @inheritParams causnet_search
#'
#' @return A list with elements `network` (output of `sink2net()`),
#'     `n_best_parents` and `mutliple_networks` (the objects needed by
#'     `return_all_best_networks()`).
#' @noRd
causnet_dp <- function(data, possible_parents, max_parents, score_fun) {
  # node scores, no parents
  ms <- no_parent_score(data, score_fun)

  # possible offspring
  possible_offspring <- find_possible_offspring(possible_parents)

  # all sets of possible parents
  possible_parent_sets <- find_possible_parent_sets(possible_parents,
                                                    max_parents)

  # scores for all sets of possible parents for each node
  possible_parent_set_scores <-
    score_possible_parent_sets(data, possible_parent_sets, score_fun)

  # BEST parent sets and scores for all sets of possible parents for each node
  bps1 <- best_possible_parent_sets(ms, possible_parent_sets,
                                    possible_parent_set_scores,
                                    max_parents)

  # best sinks for all possible connected components
  best_sinks <- find_best_sinks(possible_parents, ms, possible_offspring,
                                possible_parent_sets, bps1, max_parents)

  # ordered best sinks for labeled connected components
  bnsets <- find_best_network(best_sinks)

  # network edges and labeled connected components
  out <- sink2net(bnsets, possible_parents, possible_parent_sets, bps1,
                  max_parents)

  out[["mutliple_networks"]] <- list(bnsets = bnsets,
                                     possible_parents = possible_parents,
                                     possible_parent_sets = possible_parent_sets,
                                     bps1 = bps1,
                                     max_parents = max_parents)
  out
}

#' feasSet reduction
#'
#' @details
#' Keeps only the nodes that have at least one candidate parent or are a
#' candidate parent of some node (the feasible set of the published CausNet
#' code, `sfun()`), and renumbers them 1, ..., m in their original order. The
#' dropped nodes have neither candidate parents nor candidate offspring, so
#' they cannot be part of any edge; the search on the reduced problem is the
#' same search, only on fewer nodes.
#'
#' @param possible_parents A list of integers, with `NULL` for nodes without
#'     candidate parents.
#'
#' @return A list with `feas`, the sorted indices of the kept nodes, and
#'     `possible_parents`, the candidate parents of the kept nodes in the new
#'     numbering (element `i` belongs to node `feas[i]`).
#' @noRd
feas_reduce <- function(possible_parents) {
  has_parents <- !vapply(possible_parents, is.null, logical(1))
  feas <- sort(unique(c(which(has_parents),
                        unlist(possible_parents[has_parents]))))

  pp <- vector("list", length(feas))
  for (i in seq_along(feas)) {
    parents <- possible_parents[[feas[i]]]
    if (!is.null(parents)) pp[[i]] <- match(parents, feas)
  }
  list(feas = feas, possible_parents = pp)
}
