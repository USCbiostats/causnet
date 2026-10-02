#' Find best network
#'
#'
#' @param bsinks data.frame, output from `best_sinks`.
#' @param m interger, number of variables in data.
#'
#' @return a data.frame with 4 variables, wscore, windx, k, sink.
#' @noRd
find_best_network <- function(bsinks) {
  bestnets <- create_sink_list(
    windx = list(),
    k = numeric(),
    sink = numeric(),
    wscore = numeric(),
    attr(bsinks, "m"))

  ## Order best sinks, removing a sink at each step
  # Loop over connected components
  mycomp <- 1
  while (attr(bsinks, "index") > 0) {

    if (all(bsinks$k < 2)) break

    k <- max(bsinks$k)

    aa <- match(k, bsinks$k)
    if (aa > attr(bsinks, "index")) {
      stop("Internal error in find_best_network(): the largest `k` is in row ",
           aa, ", beyond the ", attr(bsinks, "index"),
           " valid rows of the sink list.", call. = FALSE)
    }

    bestnets <- append_sink_list(bestnets,
                                 bsinks$windx[aa],
                                 bsinks$k[aa],
                                 bsinks$sink[aa],
                                 mycomp)

    myw <- bsinks$windx[[aa]]
    w1 <- myw[!is.element(myw, bsinks$sink[aa])]

    wlen <- length(w1)
    if (wlen == 0) break

    for (d in seq_down_to_2(wlen)) {
      aa <- which(purrr::map_lgl(bsinks$windx, ~ identical(.x, w1)))

      if(length(aa) == 0) break

      bestnets <- append_sink_list(bestnets,
                                   bsinks$windx[aa[1]],
                                   bsinks$k[aa[1]],
                                   bsinks$sink[aa[1]],
                                   mycomp)

      # `w_d` is the current set of the chain walk; `myw` must keep the
      # largest set of the component for the removal step below.
      w_d <- bsinks$windx[aa][[1]]
      w1 <- w_d[!is.element(w_d, bsinks$sink[aa[1]])]
      if (length(w1) == 0) break
    }

    # remove all rows in bsinks with sets that have elements of the largest w
    # in bestnets (`myw`), so that no later component reuses its nodes
    remove_indexes <-
      which(!purrr::map_lgl(bsinks$windx, ~ sum(is.element(.x, myw)) == 0))

    bsinks <- chop_sink_list(bsinks, remove_indexes)

    mycomp <- mycomp + 1

  }

  cut_and_order_sink_list(bestnets)
}
