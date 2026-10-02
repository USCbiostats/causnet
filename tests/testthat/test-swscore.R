# A node s can be the sink of a set w even when it has more than max_parents
# candidate parents in w: it then gets the best subset of them with at most
# max_parents members, as in the published CausNet code (the fork gave such a
# sink the score Inf and dropped it).

test_that("swscore() gives a sink with too many candidate parents in w its best subset", {
  set.seed(11)
  n <- 200
  d <- data.frame(V1 = rnorm(n), V2 = rnorm(n), V3 = rnorm(n))
  d$V4 <- 0.8 * d$V1 + 0.6 * d$V2 + 0.3 * d$V3 + rnorm(n)
  pp <- list(NULL, NULL, NULL, 1:3)
  ms <- no_parent_score(d, score_bic_lm)
  pps <- find_possible_parent_sets(pp, 2)
  bps <- best_possible_parent_sets(
    ms, pps, score_possible_parent_sets(d, pps, score_bic_lm), 2
  )

  res <- swscore(4, 1:3, pp, pps, bps, 2)

  # exhaustive: every subset of {1, 2, 3} with at most 2 members
  subsets <- list(integer(), 1L, 2L, 3L, c(1L, 2L), c(1L, 3L), c(2L, 3L))
  scores <- vapply(subsets, function(pa) {
    score_bic_lm(4, if (length(pa) > 0) pa else NA, d)
  }, numeric(1))
  expect_equal(res[[2]], max(scores))
  expect_equal(sort(res[[1]]), subsets[[which.max(scores)]])
})

test_that("with every other node as candidate parent, the search reaches all 4 nodes", {
  d <- sparse_dag_data(4, seed = 5, coef = 0.8)
  pp <- lapply(1:4, function(v) setdiff(1:4, v))

  out <- causnet_search(d, pp, 2, score_bic_lm)

  bn <- out$mutliple_networks$bnsets
  top <- which.max(bn$k)
  expect_equal(bn$k[top], 4)
  expect_setequal(bn$windx[[top]], 1:4)
  expect_true(all(out$network$component == 1))
})

test_that("the total score equals the best over complete generational orderings", {
  # brute force: best_generational_score() in helper-graph.R
  cases <- list(
    list(p = 4, seed = 5, cand = "all"), list(p = 4, seed = 8, cand = "all"),
    list(p = 5, seed = 6, cand = "all"), list(p = 6, seed = 2, cand = "all"),
    list(p = 4, seed = 6, cand = "alpha"), list(p = 5, seed = 3, cand = "alpha"),
    list(p = 6, seed = 3, cand = "alpha"), list(p = 6, seed = 8, cand = "alpha")
  )
  for (cs in cases) {
    d <- sparse_dag_data(cs$p, seed = cs$seed, coef = 0.8)
    if (cs$cand == "all") {
      pp <- lapply(seq_len(cs$p), function(v) setdiff(seq_len(cs$p), v))
      out <- causnet_search(d, pp, 2, score_bic_lm)
    } else {
      pp <- find_possible_parents(d, 0.01)
      out <- causnet(d, alpha = 0.01)
    }
    best <- best_generational_score(d, pp, max_parents = 2)

    expect_true(is.finite(best))  # a complete generational ordering exists
    expect_equal(network_total_score(d, out$network), best)
  }
})
