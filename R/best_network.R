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