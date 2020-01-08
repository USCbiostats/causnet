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
  lapply(
    seq_along(pps),
    function(vertex) {
      ps_score(pps[[vertex]], vertex, data)
    }
  )
}

ps_score <- function(ps, vertex, data) {
  vapply(ps, function(x) score_bic_lm(vertex, x, data),
         FUN.VALUE = numeric(1))
}

## Helpers to transter from pps as list of lists of matrices to list of lists
## of vectors
ppss2old <- function(ppss, pps) {
  lapply(seq_along(ppss),
         function(index) {
           unname(
             split(
               ppss[[index]],
               rep(seq_along(pps[[index]]),
                   times = vapply(pps[[index]], ncol, FUN.VALUE = numeric(1)))
             )
           )
         })
}
