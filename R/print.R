#' @export
print.causnet <- function(x, ...) {
  x$mutliple_networks <- NULL
  x <- unclass(x)
  print(x)
}
