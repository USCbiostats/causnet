#' plot network
#'
#' @param x output from `causnet`.
#'
#' @return a plot of the network
#' @importFrom graphics plot
#' @export
netplot_jm <- function(x) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop('igraph is required. Please install with `install.packages("igraph")`')
  }

  mynet <- igraph::graph_from_edgelist(as.matrix(x$network[c("from", "to")]))
  plot(mynet)
}
