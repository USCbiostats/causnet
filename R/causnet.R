#' Find globally optimal causal network
#'
#' @details
#' Candidate parents are identified with a Pearson correlation test
#' (\code{cor.test()} p-value below \code{alpha}). If \code{outcome} is given,
#' the search is phenotype driven: starting from the outcome, candidate-parent
#' links are followed for at most \code{n_levels} hops, and only the nodes
#' reached this way keep candidate parents (those among the reached nodes);
#' the network is then learned on that neighbourhood only. The nodes of the
#' last level are not expanded further, but they also keep their candidate
#' parents among the reached nodes (for example a node closer to the
#' outcome). With \code{outcome = NULL} the whole graph is searched.
#'
#' Before the dynamic programme, nodes that have no candidate parents and are
#' not a candidate parent of any node are removed (the feasible set of the
#' published CausNet code). Such nodes cannot be part of any edge; the search
#' on the remaining nodes finds the same best sinks with the same scores.
#' \code{score_fun} is called on the remaining columns of \code{data}, and the
#' node indices it receives refer to those columns.
#'
#' @param data A data.frame of numeric variables.
#' @param alpha Numerical, p-value threshold to determine cutoff for possible
#'   parents.
#' @param max_parents Numeric, maximal number of parents. Defaults to 2.
#' @param score_fun A function used for scoring parent sets. For more details
#' see score_fun vignette.
#' @param outcome \code{NULL} (default) to search the whole graph, or the
#'   column index or column name of the outcome variable for a
#'   phenotype-driven search.
#' @param n_levels Integer, number of hops from \code{outcome} in the
#'   phenotype-driven search. Must be a single whole number >= 1; only used
#'   when \code{outcome} is not \code{NULL}. Defaults to 3.
#'
#' @return An object of class \code{causnet}, a list with 3 elements:
#' \code{network}, a data.frame with 3 columns \code{from}, \code{to} and
#' \code{component}; \code{n_best_parents}, the number of parents in the best
#' parent set of each sink; and \code{mutliple_networks}, internal objects
#' used by \code{\link{return_all_best_networks}} and
#' \code{\link{return_random_best_networks}} (both deprecated). Node indices
#' in \code{from} and \code{to} are column indices of \code{data}.
#' @export
#'
#' @examples
#' set.seed(1)
#' new_data <- simdat(n_var = 5)
#' out <- causnet(data = new_data, 0.5)
#'
#' # phenotype-driven search around the last variable
#' out <- causnet(data = new_data, 0.5, outcome = 5, n_levels = 2)
causnet <- function(data, alpha = 0.05, max_parents = 2, score_fun = NULL,
                    outcome = NULL, n_levels = 3) {

  if (length(alpha) != 1 || !is.numeric(alpha) || alpha < 0 || alpha > 1) {
    stop("`alpha` must be a single numeric value in [0, 1].", call. = FALSE)
  }
  outcome <- outcome_index(outcome, data)
  check_n_levels(n_levels)

  if (is.null(score_fun)) {
    score_fun <- score_bic_lm
  }

  possible_parents <- find_possible_parents(data, alpha)

  causnet_search(data, possible_parents, max_parents, score_fun,
                 outcome, n_levels)
}
