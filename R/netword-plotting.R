#' plot network
#'
#' @param mylinks data.frame with 3 columns; from, to and component
#'
#' @return a plot of the network
#' @importFrom graphics plot
#' @export
netplot_jm = function(mylinks) {

  if (!requireNamespace('igraph', quietly = TRUE)) {
    stop('igraph is required. Please install with `install.packages("igraph")`')
  }

  mynodes = unique(c(mylinks[,"from"], mylinks[,"to"]))
  mynodes = mynodes[order(mynodes)]
  mynet = igraph::graph.data.frame(mylinks, mynodes, directed=T) # igraph
  plot(mynet, edge.arrow.size=.4, edge.color="grey50",
       vertex.color="gold", vertex.frame.color="darkred",
       vertex.label=igraph::V(mynet), vertex.label.color="black")
}
