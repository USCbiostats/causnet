#' Score possible parent sets
#'
#' This function looks at all the possible parent sets a give each of them a
#' score.
#'
#' @param data A data.frame of numeric variables.
#' @param pps Possible parent sets, output from `find_possible_parent_sets`.
#' @param score_fun scoring function.
#'
#' @return A list of lists of vectors of numerics.
#' @noRd
score_possible_parent_sets <- function(data, pps, score_fun) {
  purrr::imap(pps, ~ps_score(.x, .y, data, score_fun))
}

ps_score <- function(ps, vertex, data, score_fun) {
  map_dbl(ps, ~score_fun(vertex, .x, data))
}
