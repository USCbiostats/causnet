# find_best_network() unrolls one connected component at a time and then
# removes every sink-list row that touches the component's largest node set.
# If a smaller set is used for the removal, later "components" reuse nodes of
# earlier ones and the returned edges can contain both a -> b and b -> a.
# has_two_cycle() and is_dag() are in helper-graph.R.

test_that("find_best_network() returns components with disjoint node sets", {
  set.seed(1)
  d <- simdat(sample.size = 200, n_var = 6, beta = 0.3)
  ms <- no_parent_score(d, score_bic_lm)
  pp <- find_possible_parents(d, 0.05)
  pps <- find_possible_parent_sets(pp, 2)
  bps <- best_possible_parent_sets(
    ms, pps, score_possible_parent_sets(d, pps, score_bic_lm), 2
  )
  bs <- find_best_sinks(pp, ms, find_possible_offspring(pp), pps, bps, 2)

  bn <- find_best_network(bs)

  comp_nodes <- lapply(split(bn$windx, bn$wscore), function(w) unique(unlist(w)))
  expect_gt(length(comp_nodes), 0)
  expect_equal(anyDuplicated(unlist(comp_nodes)), 0)
})

test_that("causnet() returns a DAG without 2-cycles", {
  set.seed(1)
  d <- simdat(sample.size = 200, n_var = 6, beta = 0.3)
  set.seed(1)
  net <- causnet(d, alpha = 0.05, max_parents = 2)$network

  expect_gt(nrow(net), 0)
  expect_false(has_two_cycle(net))
  expect_true(is_dag(net))
})

test_that("causnet_stable() returns a DAG without 2-cycles", {
  set.seed(1)
  d <- simdat(sample.size = 200, n_var = 8, beta = 0.3)
  set.seed(1)
  net <- suppressMessages(
    causnet_stable(d, tau = 4, pi_thr = 0.6, B = 50, seed = 1)
  )$network

  expect_gt(nrow(net), 0)
  expect_false(has_two_cycle(net))
  expect_true(is_dag(net))
})

test_that("disconnected components are all returned", {
  set.seed(1)
  d <- cbind(simdat(sample.size = 200, n_var = 3, beta = 0.6),
             simdat(sample.size = 200, n_var = 3, beta = 0.6))
  names(d) <- paste0("V", 1:6)

  net <- causnet(d, alpha = 0.001, max_parents = 2)$network

  in_block_a <- net$from <= 3 & net$to <= 3
  in_block_b <- net$from > 3 & net$to > 3
  expect_true(any(in_block_a))
  expect_true(any(in_block_b))
  expect_true(all(in_block_a | in_block_b))
  expect_gte(length(unique(net$component)), 2)
})
