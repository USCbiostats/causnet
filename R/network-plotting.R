#' plot network
#'
#' @param mylinks data.frame with 3 columns; from, to and component
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
