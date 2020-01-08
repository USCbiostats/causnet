#' Bigger is better score for node y and parents x
#' @keywords internal
#' @importFrom stats BIC lm
score_bic_lm <- function(y, x, mydat) {
  y_nm <- colnames(mydat)[y]
  if (is.element(x[1], seq_len(ncol(mydat)))) {
    x_nms <- colnames(mydat)[x]
  } else {
    x_nms <- "1"
  }
  fit <- lm(paste0(y_nm, " ~ ", paste(x_nms, collapse = " + ")), data = mydat)
  bic <- - (1 / 2) * BIC(fit)
  return(bic)
}

# get edges of best network connected components from ordered sinks
sink2net <- function(bnets, pp, pps, bps) {
  m <- length(bps[[1]])
  nms <- c("node.source", "node.sink", "component")
  mynets <- as.data.frame(matrix(NA, nrow = 0, ncol = length(nms)))
  names(mynets) <- nms
  n_comp <- length(unique(bnets$component))
  rowno <- 1
  n_conflicts <- integer()

  for (c_index in seq_len(n_comp)) {
    tmpn <- bnets[is.element(bnets$component, c_index), ]
    for (k in max(tmpn$k):2) {
      bp_set <- NULL
      tmp <- tmpn[is.element(tmpn$k, k), ]
      s <- tmp[1, "sink"]
      w <- subsetur(m, tmp[1, "windx"])
      bp_set <- swscore(s, w, pp, pps, bps)[[1]]
      if (!is.null(bp_set[[1]])) {
        src <- bp_set
        n_conflicts <- c(n_conflicts, length(src[[1]]))
        # Sampling 1 best sink at random
        src[[1]] <- src[[1]][sample.int(length(src[[1]]), 1)]

        snk <- rep(s, length(bp_set))
        cmp <- rep(c_index, length(bp_set))
        mynets[rowno:(rowno + length(bp_set) - 1), ] <- cbind(src, snk, cmp)
        rowno <- rowno + length(bp_set)
      }
    }
  }
  return(list(network = mynets,
              n_best_parents = n_conflicts))
} # end sink2net
