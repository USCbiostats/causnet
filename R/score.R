#' Compute scores for nodes with no parents
#'
#' Applies score_bic_lm to each of the columns in the data frames with no
#' parents.
#'
#' @param data Data.frame.
#'
#' @return Numeric vector, same length as number of variables in `data`.
#' @noRd
#' @importFrom purrr map_dbl
no_parent_score <- function(data) {
  map_dbl(seq_len(ncol(data)), score_bic_lm, x = NA, mydat = data)
}
