#' Main function
#'
#' @param mydata data.frame
#'
#' @return data.frame with 3 columns; from, to and component
#' @export
causnet <- function(mydata) {
  n_var <- ncol(mydata)

  # node scores, no parents
  ms <- mscores(1:n_var, mydata)
  pp <- mypp(mydata, 0.05, n_var)

  # possible offspring
  po <- pofun(pp)

  # all sets of possible parents
  pps <- pp_sets(pp)

  # scores for all sets of possible parents for each node
  ppss <- pp_sets_s(mydata, pps)

  # BEST parent sets and scores for all sets of possible parents for each node
  bps <- pp_sets_bs(pps, ppss, ms)

  # best sinks for all possible connected components
  bsinksc <- bestSinksCnew(pp, ms, po, pps, ppss, bps, mydata)

  # ordered best sinks for labeled connected components
  bnets <- bestnet(bsinksc, n_var)

  # network edges and labeled connected components
  mylinks <- sink2net(bnets, pp, pps, bps)
  names(mylinks[[1]])[1:2] <- c("from", "to")
  return(mylinks)
}
