#' Find best network
#'
#'
#' @param bsinks data.frame, output from `best_sinks`.
#' @param m interger, number of variables in data.
#'
#' @return a data.frame with 4 variables, wscore, windx, k, sink.
#' @noRd
find_best_network <- function(bsinks, m) {
  nms <- c("windx", "k", "sink", "component")
  bestnets <- as.data.frame(matrix(NA, nrow = 0, ncol = length(nms)))
  names(bestnets) <- nms

  ## Order best sinks, removing a sink at each step
  # Loop over connected components
  mycomp <- 1
  rowno <- 1
  while (nrow(bsinks) > 0) {
    ks <- unique(bsinks$k)
    ks <- ks[order(ks, decreasing = TRUE)]
    k <- max(ks)
    aa <- match(k, bsinks$k)
    tmp_s <- bsinks[aa, ]
    bestnets[rowno, nms[1:3]] <- tmp_s[1, nms[1:3]]
    bestnets[rowno, "component"] <- mycomp
    myw <- subsetur(m, tmp_s[1, "windx"])
    w1 <- myw[!is.element(myw, tmp_s[1, "sink"])]
    w1indx <- subsetr(m, w1)

    rowno <- rowno + 1
    wlen <- length(w1)
    for (d in wlen:2) {
      aa <- match(w1indx, bsinks$windx)
      tmp_s <- bsinks[aa, ]
      bestnets[rowno, nms[1:3]] <- tmp_s[1, nms[1:3]]
      bestnets[rowno, "component"] <- mycomp
      w <- subsetur(m, tmp_s[1, "windx"])
      w1 <- w[!is.element(w, tmp_s[1, "sink"])]
      w1indx <- subsetr(m, w1)
      rowno <- rowno + 1
    }

    # remove all rows in bsinks with sets that have elements of the largest w
    # in bestnets
    aa <- NULL
    for (j in seq_len(nrow(bsinks))) {
      wj <- subsetur(m, bsinks[j, "windx"])
      aa <- c(aa, sum(is.element(wj, myw)) == 0)
    }
    bsinks <- bsinks[aa, ]
    mycomp <- mycomp + 1
  } # end while loop

  return(bestnets)
} # end bestnet


find_best_network_2 <- function(bsinks) {
  bestnets <- create_sink_list(
    windx = list(),
    k = numeric(),
    sink = numeric(),
    wscore = numeric(),
    5)

  ## Order best sinks, removing a sink at each step
  # Loop over connected components
  mycomp <- 1
  while (attr(bsinks, "index") > 0) {
    k <- max(bsinks$k)

    aa <- match(k, bsinks$k)

    bestnets <- append_sink_list(bestnets,
                                 bsinks$windx[aa],
                                 bsinks$k[aa],
                                 bsinks$sink[aa],
                                 mycomp)

    myw <- bsinks$windx[[aa]]
    w1 <- myw[!is.element(myw, bsinks$sink[aa])]

    wlen <- length(w1)
    if (wlen == 0) break
    for (d in wlen:2) {
      aa <- which(purrr::map_lgl(bsinks$windx, ~ identical(.x, w1)))

      bestnets <- append_sink_list(bestnets,
                                   bsinks$windx[aa[1]],
                                   bsinks$k[aa[1]],
                                   bsinks$sink[aa[1]],
                                   mycomp)

      myw <- bsinks$windx[aa][[1]]
      w1 <- myw[!is.element(myw, bsinks$sink[aa[1]])]
    }

    # remove all rows in bsinks with sets that have elements of the largest w
    # in bestnets
    remove_indexes <-
      which(!purrr::map_lgl(bsinks$windx, ~ sum(is.element(.x, myw)) == 0))

    bsinks <- chop_sink_list(bsinks, remove_indexes)
    mycomp <- mycomp + 1
  }

  cut_and_order_sink_list(bestnets)
}
