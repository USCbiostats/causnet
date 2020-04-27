#' Compute scores for nodes with no parents
#'
#' Applies score_bic_lm to each of the columns in the data frames with no
#' parents.
#'
#' @param data Data.frame.
#' @param score_fun scoring function
#'
#' @return Numeric vector, same length as number of variables in `data`.
#' @noRd
#' @importFrom purrr map_dbl
no_parent_score <- function(data, score_fun) {
  map_dbl(seq_len(ncol(data)), score_fun, x = NA, mydat = data)
}
