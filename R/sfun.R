#' Main function
#'
#' @param mydata data.frame
#'
#' @return data.frame with 3 columns; from, to and component
#' @export
sfun = function(mydata){
  n.var = ncol(mydata)
  ms = mscores(1:n.var, mydata) # node scores, no parents
  pp = mypp(mydata, 0.05, n.var)
  po = pofun(pp) # possible offspring
  pps = pp.sets(pp) # all sets of possible parents
  ppss = pp.sets.s(mydata, pps) # scores for all sets of possible parents for each node
  bps = pp.sets.bs(pps, ppss, ms) # BEST parent sets and scores for all sets of possible parents for each node

  bsinks = bestSinks(pp, ms, po, pps, ppss, bps, mydata) # best sinks for all possible connected components
  browser()
  bnets = bestnet(bsinks, n.var) # ordered best sinks for labeled connected components
  mylinks = sink2net(bnets, pp, pps, bps) # network edges and labeled connected components
  names(mylinks)[1:2] = c("from", "to")
  return(mylinks)
} # end sfun
