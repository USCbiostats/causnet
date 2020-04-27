create_sink_list <- function(windx, k, sink, wscore, m) {
  length_out <-  m ^ 4

  out <- new.env()
  out$windx  = list()
  out$k       = numeric(length_out)
  out$sink   = numeric(length_out)
  out$wscore = numeric(length_out)
  attr(out, "m") <- m
  attr(out, "length") <- length_out
  attr(out, "index") <- length(k)

  if (length(windx) > 0) {
    out$windx[seq_len(m)] <- windx
    out$k[seq_len(m)] <- k
    out$sink[seq_len(m)] <- sink
    out$wscore[seq_len(m)] <- wscore
  }
  out
}

sink_score_one_node <- function(m, ms) {
  s <- seq_len(m)
  create_sink_list(windx = s,
                   k = rep(1, m),
                   sink = s,
                   wscore = ms[s],
                   m = m)
}

append_sink_list <- function(sink_list, windx, k, sink, wscore) {
  input_length <- length(windx)
  index_range <- seq_len(input_length) + attr(sink_list, "index")

  sink_list$windx <- c(sink_list$windx, windx)
  sink_list$k[index_range] <- k
  sink_list$sink[index_range] <- sink
  sink_list$wscore[index_range] <- wscore

  attr(sink_list, "index") <- attr(sink_list, "index") + input_length
  sink_list
}

remove_dublicates <- function(sink_list) {
  values <- purrr::map2_chr(
    sink_list$windx,
    sink_list$sink[seq_len(attr(sink_list, "index"))],
    ~ paste0(paste(sort(.x), collapse = "-"), "_", .y)
    )

  dups <- duplicated(values)

  if (all(!dups)) return(sink_list)

  keeps <- which(!duplicated(values))

  sink_list$windx <- map(sink_list$windx[keeps], sort)
  sink_list$k[seq_along(keeps)] <- sink_list$k[keeps]
  sink_list$sink[seq_along(keeps)] <- sink_list$sink[keeps]
  sink_list$wscore[seq_along(keeps)] <- sink_list$wscore[keeps]

  sink_list$k[seq(length(keeps) + 1, attr(sink_list, "length"))] <- 0
  sink_list$sink[seq(length(keeps) + 1, attr(sink_list, "length"))] <- 0
  sink_list$wscore[seq(length(keeps) + 1, attr(sink_list, "length"))] <- 0

  attr(sink_list, "index") <- length(keeps)
  sink_list
}

set_sink_list <- function(sink_list, windx, k, sink, wscore) {
  input_length <- length(windx)

  sink_list$windx <- windx
  sink_list$k[seq_len(input_length)] <- k
  sink_list$sink[seq_len(input_length)] <- sink
  sink_list$wscore[seq_len(input_length)] <- wscore

  sink_list$k[seq(input_length + 1, attr(sink_list, "length"))] <- 0
  sink_list$sink[seq(input_length + 1, attr(sink_list, "length"))] <- 0
  sink_list$wscore[seq(input_length + 1, attr(sink_list, "length"))] <- 0

  attr(sink_list, "index") <- input_length
  sink_list
}

cut_and_order_sink_list <- function(sink_list) {
  indexes <- seq_len(attr(sink_list, "index"))

  sink_list$k <- sink_list$k[indexes]
  sink_list$sink <- sink_list$sink[indexes]
  sink_list$wscore <- sink_list$wscore[indexes]

  new_order <- order(sink_list$wscore, sink_list$sink)

  sink_list$windx <- sink_list$windx[new_order]
  sink_list$k <- sink_list$k[new_order]
  sink_list$sink <- sink_list$sink[new_order]
  sink_list$wscore <- sink_list$wscore[new_order]
  sink_list
}

## Remove section and slide rest over ------------------------------------------

fast_setdiff <- function(a, b) {
  a[-b]
}

chop_sink_list <- function(sink_list, indexes) {
  sink_list$windx <- sink_list$windx[-indexes]

  index <- attr(sink_list, "index")

  indexes_length <- length(indexes)

  store_indexes <- seq_len(attr(sink_list, "index") - indexes_length)
  pluck_indexes <- fast_setdiff(seq_len(attr(sink_list, "index")), indexes)
  zero_indexes <- index - indexes_length + seq_len(indexes_length)

  sink_list$k[store_indexes] <- sink_list$k[pluck_indexes]
  sink_list$k[zero_indexes] <- 0

  sink_list$sink[store_indexes] <- sink_list$sink[pluck_indexes]
  sink_list$sink[zero_indexes] <- 0

  sink_list$wscore[store_indexes] <- sink_list$wscore[pluck_indexes]
  sink_list$wscore[zero_indexes] <- 0

  attr(sink_list, "index") <- attr(sink_list, "index") - indexes_length
  sink_list
}
