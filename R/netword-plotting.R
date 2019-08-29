#' plot network
#'
#' @param mylinks data.frame with 3 columns; from, to and component
#'
#' @return a plot of the network
#' @export
netplot_jm = function(mylinks){
  require(igraph)
  mynodes = unique(c(mylinks[,"from"], mylinks[,"to"]))
  mynodes = mynodes[order(mynodes)]
  mynet = graph.data.frame(mylinks, mynodes, directed=T) # igraph
  plot(mynet, edge.arrow.size=.4, edge.color="grey50",
       vertex.color="gold", vertex.frame.color="darkred",
       vertex.label=V(mynet), vertex.label.color="black")
}
