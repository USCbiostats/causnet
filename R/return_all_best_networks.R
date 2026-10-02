#' Return all best networks (deprecated)
#'
#' @description
#' Deprecated: every call gives a warning; the returned value is unchanged.
#' This function enumerates the networks that take one parent from the single
#' best parent set of each sink; these are not genuinely tied best networks.
#' \code{\link{causnet}} and \code{\link{causnet_stable}} now return every
#' parent of each best parent set, so these networks are no longer
#' alternative best networks.
#'
#' @param input Result of \code{\link{causnet}} or
#'   \code{\link{causnet_stable}}.
#'
#' @return A list of networks (data.frames with columns \code{node.source},
#'   \code{node.sink} and \code{component}), one for each way of taking one
#'   parent from the best parent set of each sink.
#' @export
#'
#' @examples
#' set.seed(1234)
#' mydata <- simdat(n_var = 10)
#' out <- causnet(data = mydata, max_parents = 3)
#'
#' # deprecated: gives a warning (suppressed here)
#' suppressWarnings(return_all_best_networks(out))
return_all_best_networks <- function(input) {
  .Deprecated(
    msg = best_networks_deprecation_msg("return_all_best_networks")
  )

  # Generate all paths
  x <- unname(t(expand.grid(lapply(input$n_best_parents, seq_len))))
  all_paths <- lapply(seq_len(ncol(x)), function(i) x[,i])


  lapply(all_paths, best_networks_sink2net, input$mutliple_networks)
}

#' Return n random best networks (deprecated)
#'
#' @description
#' Deprecated: every call gives a warning; the returned value is unchanged.
#' This function draws from the networks that take one parent from the single
#' best parent set of each sink; these are not genuinely tied best networks.
#' \code{\link{causnet}} and \code{\link{causnet_stable}} now return every
#' parent of each best parent set, so these networks are no longer
#' alternative best networks.
#'
#' @param input Result of \code{\link{causnet}} or
#'   \code{\link{causnet_stable}}.
#' @param n Number of networks to return.
#'
#' @return A list of \code{n} of the networks returned by
#'   \code{\link{return_all_best_networks}}, drawn at random without
#'   replacement; all of them (and a printed note) if \code{n} is larger than
#'   their number.
#' @export
#'
#' @examples
#' set.seed(1234)
#' mydata <- simdat(n_var = 10)
#' out <- causnet(data = mydata, max_parents = 3)
#'
#' # deprecated: gives a warning (suppressed here)
#' suppressWarnings(return_random_best_networks(out, 1))
return_random_best_networks <- function(input, n) {
  .Deprecated(
    msg = best_networks_deprecation_msg("return_random_best_networks")
  )

  # Generate all paths
  x <- unname(t(expand.grid(lapply(input$n_best_parents, seq_len))))
  all_paths <- lapply(seq_len(ncol(x)), function(i) x[,i])

  if (n > length(all_paths)) {
    cat("Returning", length(all_paths), "best networks")
  } else {
    all_paths <- all_paths[sample.int(length(all_paths), n)]
  }

  lapply(all_paths, best_networks_sink2net, input$mutliple_networks)
}

# message of the deprecation warning of the two functions above
best_networks_deprecation_msg <- function(fun) {
  paste0(
    fun, "() is deprecated. It enumerates networks that take one parent ",
    "from the single best parent set of each sink; these are not genuinely ",
    "tied best networks. causnet() and causnet_stable() now return every ",
    "parent of each best parent set, so these networks are no longer ",
    "alternative best networks."
  )
}

best_networks_sink2net <- function(network_path, data) {
  bnets <- data[[1]]
  pp <- data[[2]]
  pps <- data[[3]]
  bps <- data[[4]]
  max_parents <- data[[5]]

  # TODO change wscore to component in name only
  mynets <- data.frame(node.source = numeric(),
                       node.sink = numeric(),
                       component = numeric())
  n_comp <- length(unique(bnets$wscore)) # component
  rowno <- 1
  n_conflicts <- integer()

  network_path_index <- 1

  for (c_index in seq_len(n_comp)) {
    indexes <- bnets$wscore == c_index
    for (k in seq_down_to_2(max(bnets$k[indexes]))) {
      bp_set <- NULL
      tmp <- which(indexes)[bnets$k[indexes] == k]
      if(length(tmp) == 0) break

      s <- bnets$sink[tmp][[1]]
      w <- bnets$windx[tmp][[1]]

      if (is.null(w)) next
      bp_set <- swscore(s, w, pp, pps, bps, max_parents)[[1]]

      if (is.null(bp_set)) next
      if (any(is.infinite(bp_set))) next

      src <- bp_set
      n_conflicts <- c(n_conflicts, length(src))


      src <- src[network_path[network_path_index]]
      network_path_index <- network_path_index + 1

      mynets[rowno, ] <- cbind(src, s, c_index)
      rowno <- rowno + 1
    }
  }

  # causnet() searches a reduced problem; `feas` maps its node indices back
  # to the columns of the data
  mynets$node.source <- as.numeric(data$feas[mynets$node.source])
  mynets$node.sink <- as.numeric(data$feas[mynets$node.sink])
  mynets
}
