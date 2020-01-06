#' Find globally optimal causal network
#'
#' @param data A data.frame of numeric variables.
#' @param alpha Numerical, p-value threshold to determine cutoff for possible
#'   parents.
#'
#' @return A list with 2 elements: a data data.frame with 3 columns; from, to
#' and component, and a vector of number of best parents.
#' @export
#'
#' @examples
#' new_data <- simdat(n_var = 5)
#' out <- causnet(data = new_data, 0.5)
causnet <- function(data, alpha = 0.05) {
  n_var <- ncol(data)

  if (length(alpha) != 1 || !is.numeric(alpha) || alpha < 0 || alpha > 1) {
    stop("`alpha` must be a single numeric value in [0, 1].", call. = FALSE)
  }

  # node scores, no parents
  ms <- no_parent_score(data)
  possible_parents <- find_possible_parents(data, alpha)

  # possible offspring
  po <- pofun(possible_parents)

  # all sets of possible parents
  pps <- pp_sets(possible_parents)

  # scores for all sets of possible parents for each node
  ppss <- pp_sets_s(data, pps)

  # BEST parent sets and scores for all sets of possible parents for each node
  bps <- pp_sets_bs(pps, ppss, ms)

  # best sinks for all possible connected components
  bsinksc <- bestSinksCnew(possible_parents, ms, po, pps, ppss, bps, data)

  # ordered best sinks for labeled connected components
  bnets <- bestnet(bsinksc, n_var)

  # network edges and labeled connected components
  out <- sink2net(bnets, possible_parents, pps, bps)
  names(out[[1]])[1:2] <- c("from", "to")
  out
}
