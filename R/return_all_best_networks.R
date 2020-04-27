#' Return all best networks
#'
#' @param input Result from [caunset] function
#'
#' @return list of best networks
#' @export
#'
#' @examples
#' set.seed(1234)
#' mydata <- simdat(n_var = 10)
#' out <- causnet(data = mydata, max_parents = 3)
#'
#' return_all_best_networks(out)
return_all_best_networks <- function(input) {

  # Generate all paths
  x <- unname(t(expand.grid(lapply(input$n_best_parents, seq_len))))
  all_paths <- lapply(seq_len(ncol(x)), function(i) x[,i])


  lapply(all_paths, best_networks_sink2net, input$mutliple_networks)
}

#' Return n random best networks
#'
#' @param input Result from [caunset] function
#' @param n numeric
#'
#' @return list of best networks
#' @export
#'
#' @examples
#' set.seed(1234)
#' mydata <- simdat(n_var = 10)
#' out <- causnet(data = mydata, max_parents = 3)
#'
#' return_random_best_networks(out, 1)
return_random_best_networks <- function(input, n) {

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
    for (k in max(bnets$k[indexes]):2) {
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

  mynets
}
