# feasSet reduction: before the dynamic programme, causnet() and
# causnet_stable() remove every node that has no candidate parents and is not
# a candidate parent of any node, and renumber the remaining nodes.

# (set, sink, score) of every row of the best-sink list, with node indices
# translated by `map` (index in `d` -> column index of the full data)
best_sink_rows <- function(d, pp, map = seq_len(ncol(d))) {
  ms <- no_parent_score(d, score_bic_lm)
  pps <- find_possible_parent_sets(pp, 2)
  bps <- best_possible_parent_sets(
    ms, pps, score_possible_parent_sets(d, pps, score_bic_lm), 2
  )
  bs <- find_best_sinks(pp, ms, find_possible_offspring(pp), pps, bps, 2)
  n <- attr(bs, "index")
  out <- data.frame(
    set = vapply(bs$windx[seq_len(n)],
                 function(w) paste(sort(map[w]), collapse = "-"), ""),
    sink = map[bs$sink[seq_len(n)]],
    wscore = bs$wscore[seq_len(n)]
  )
  out <- out[order(out$set, out$sink), ]
  rownames(out) <- NULL
  out
}

test_that("feas_reduce() drops unconnected nodes and renumbers the others", {
  pp <- list(NULL, 1L, NULL, NULL, c(2L, 6L), NULL, NULL)
  fr <- feas_reduce(pp)

  expect_identical(fr$feas, c(1L, 2L, 5L, 6L))
  expect_identical(fr$possible_parents, list(NULL, 1L, c(2L, 4L), NULL))
})

test_that("feas_reduce() returns an empty problem without candidate parents", {
  fr <- feas_reduce(vector("list", 4))

  expect_length(fr$feas, 0)
  expect_length(fr$possible_parents, 0)
})

test_that("the reduction does not change the dynamic programme", {
  # p = 12: nodes are dropped and the others renumbered in every dataset
  for (seed in c(1, 3, 4, 5, 6)) {
    d <- sparse_dag_data(12, seed = seed)
    pp <- find_possible_parents(d, 0.01)
    fr <- feas_reduce(pp)
    expect_lt(length(fr$feas), 12)

    full <- best_sink_rows(d, pp)
    reduced <- best_sink_rows(d[, fr$feas, drop = FALSE], fr$possible_parents,
                              map = fr$feas)
    expect_identical(reduced[c("set", "sink")], full[c("set", "sink")])
    expect_equal(reduced$wscore, full$wscore)
  }
})

test_that("with fewer than 10 variables the network is the same with and without the reduction", {
  n_renumbered <- 0
  for (seed in 3:6) {
    d <- sparse_dag_data(9, seed = seed)
    pps <- list(
      find_possible_parents(d, 0.01),
      suppressMessages(find_possible_parents_stable(d, tau = 2, B = 30,
                                                    seed = seed))
    )
    for (pp in pps) {
      fr <- feas_reduce(pp)
      # a dropped node lies before a kept one, so kept nodes are renumbered
      dropped <- setdiff(1:9, fr$feas)
      if (length(dropped) > 0 && min(dropped) < max(fr$feas)) {
        n_renumbered <- n_renumbered + 1
      }

      full <- causnet_dp(d, pp, 2, score_bic_lm)
      names(full$network)[1:2] <- c("from", "to")
      reduced <- causnet_search(d, pp, 2, score_bic_lm)

      expect_identical(reduced$network, full$network)
      expect_identical(reduced$n_best_parents, full$n_best_parents)
    }
  }
  expect_gte(n_renumbered, 6)
})

test_that("causnet() returns an empty network when no variable has candidate parents", {
  d <- sparse_dag_data(6, seed = 1)
  out <- causnet(d, alpha = 0)

  expect_equal(nrow(out$network), 0)
  expect_named(out$network, c("from", "to", "component"))
  expect_length(out$n_best_parents, 0)
  # deprecated; its warning is tested in test-return_best_networks.R
  expect_length(suppressWarnings(return_all_best_networks(out)), 0)
})

test_that("return_all_best_networks() uses the column indices of the data", {
  d <- sparse_dag_data(9, seed = 4)  # nodes 4 and 8 are dropped
  fr <- feas_reduce(find_possible_parents(d, 0.01))
  m <- length(fr$feas)
  expect_lt(m, 9)
  # nodes whose index in the reduced problem differs from their column index
  renumbered <- fr$feas[fr$feas != seq_len(m)]
  expect_gt(length(renumbered), 0)

  out <- causnet(d, alpha = 0.01)
  expect_true(any(c(out$network$from, out$network$to) %in% renumbered))

  nets <- suppressWarnings(return_all_best_networks(out))  # deprecated
  expect_gt(length(nets), 0)
  edges <- paste(out$network$from, out$network$to)
  for (net in nets) {
    expect_true(all(paste(net$node.source, net$node.sink) %in% edges))
  }
  # a node index above m can only come from the translation with `feas`
  nodes <- unlist(lapply(nets, function(net) c(net$node.source, net$node.sink)))
  expect_true(any(nodes > m))
})
