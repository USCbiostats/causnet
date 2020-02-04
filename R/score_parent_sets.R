#' Score possible parent sets
#'
#' This function looks at all the possible parent sets a give each of them a
#' score.
#'
#' @param data A data.frame of numeric variables.
#' @param pps Possible parent sets, output from `find_possible_parent_sets`.
#'
#' @return A list of lists of vectors of numerics.
#' @noRd
score_possible_parent_sets <- function(data, pps) {
  purrr::imap(pps, ~ps_score(.x, .y, data))
}

ps_score <- function(ps, vertex, data) {
  map_dbl(ps, ~score_bic_lm(vertex, .x, data))
}
