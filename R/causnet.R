#' Find globally optimal causal network
#'
#' @param data A data.frame of numeric variables.
#' @param alpha Numerical, p-value threshold to determine cutoff for possible
#'   parents.
#' @param max_parents Numeric, maximal number of parents. Defaults to 2.
#'
#' @return A list with 2 elements: a data data.frame with 3 columns; from, to
#' and component, and a vector of number of best parents.
#' @export
#'
#' @examples
#' set.seed(1)
#' new_data <- simdat(n_var = 5)
#' out <- causnet(data = new_data, 0.5)
causnet <- function(data, alpha = 0.05, max_parents = 2) {

  if (length(alpha) != 1 || !is.numeric(alpha) || alpha < 0 || alpha > 1) {
    stop("`alpha` must be a single numeric value in [0, 1].", call. = FALSE)
  }

  # node scores, no parents
  ms <- no_parent_score(data)
  possible_parents <- find_possible_parents(data, alpha)

  # possible offspring
  possible_offspring <- find_possible_offspring(possible_parents)

  # all sets of possible parents
  possible_parent_sets <- find_possible_parent_sets(possible_parents,
                                                    max_parents)

  # scores for all sets of possible parents for each node
  possible_parent_set_scores <-
    score_possible_parent_sets(data, possible_parent_sets)

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

  out[[1]]
  names(out[[1]])[1:2] <- c("from", "to")
  out
}
