#' Compute index r for some set T, which is a subset of a set with n elements
#' watch out for rounding errors!!!
#'
#' @param n numeric
#' @param set set of elements
#'
#' @keywords internal
#' @export
subsetr <- function(n, set) {
  r <- 0L
  for (i in seq_len(n)) {
    if (is.element(i, set)) r <- r + 2L ^ (n - i)
  }
  return(r)
}

#' Find a subset set T of a set with n elements, using index r
#' watch out for rounding errors!!!
#'
#' @param n numeric
#' @param r set of elements
#'
#' @keywords internal
#' @export
subsetur <- function(n, r) {
  set <- integer()
  for (i in n:1) {
    if ((r %% 2) == 1) set <- c(i, set)
    r <- r %/% 2
  }
  return(set)
}

# from, from - 1, ..., 2 for use as a descending loop index; empty when
# from < 2 (`from:2` would count upwards, e.g. 1:2 is c(1, 2)).
seq_down_to_2 <- function(from) {
  if (from < 2) return(integer())
  from:2
}
