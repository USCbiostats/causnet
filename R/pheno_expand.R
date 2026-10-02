#' Restrict candidate parents to the neighbourhood of an outcome
#'
#' @details
#' Phenotype-driven search: starting from `outcome`, follow candidate-parent
#' links for at most `n_levels` hops (level 1 = candidate parents of the
#' outcome, level 2 = their candidate parents, ...). Every node reached this
#' way, including the nodes of the deepest level, keeps those of its
#' candidate parents that were also reached; all other nodes get no
#' candidate parents. Nodes of the deepest level are not expanded further, so
#' their candidate parents outside the reached set are dropped.
#'
#' @param possible_parents A list of integers, output from
#'     find_possible_parents or find_possible_parents_stable.
#' @param outcome Integer, index of the outcome node.
#' @param n_levels Integer, maximal number of hops from `outcome`.
#'
#' @return A list of the same length as `possible_parents`, with `NULL` for
#'     nodes without candidate parents.
#' @noRd
pheno_expand <- function(possible_parents, outcome, n_levels) {
  reached <- outcome
  frontier <- outcome
  for (level in seq_len(n_levels)) {
    nxt <- setdiff(unique(unlist(possible_parents[frontier])), reached)
    if (length(nxt) == 0) break
    reached <- c(reached, nxt)
    frontier <- nxt
  }
  reached <- sort(unique(reached))

  out <- vector("list", length(possible_parents))
  for (v in reached) {
    keep <- intersect(possible_parents[[v]], reached)
    if (length(keep) > 0) out[[v]] <- keep
  }
  out
}

# Column index of the outcome (given as a column index or name), or NULL.
outcome_index <- function(outcome, data) {
  if (is.null(outcome)) return(NULL)
  if (is.character(outcome)) outcome <- match(outcome, colnames(data))
  if (length(outcome) != 1 || !is.numeric(outcome) ||
      !is.element(outcome, seq_len(ncol(data)))) {
    stop("`outcome` must be NULL, or a single column index or column name ",
         "of `data`.", call. = FALSE)
  }
  as.integer(outcome)
}

# Called by causnet() and causnet_stable() whether or not `outcome` is NULL.
check_n_levels <- function(n_levels) {
  if (length(n_levels) != 1 || !is.numeric(n_levels) ||
      !is.finite(n_levels) || n_levels < 1 || n_levels != round(n_levels)) {
    stop("`n_levels` must be a single whole number >= 1.", call. = FALSE)
  }
}
