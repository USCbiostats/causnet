# sink2net() must turn every parent of a sink's best parent set into an edge,
# so that causnet() is deterministic and keeps multi-parent structure.
# triangle_data() is in helper-graph.R.

test_that("causnet() keeps every parent in the best parent set", {
  d <- triangle_data()
  set.seed(1)
  out <- causnet(d, alpha = 0.05, max_parents = 2)
  net <- out$network

  # the three variables are pairwise dependent: the best network is a
  # complete DAG, i.e. one node with 2 parents and 3 edges in total
  expect_equal(nrow(net), 3)
  expect_equal(max(table(net$to)), 2)
  expect_equal(sum(out$n_best_parents), nrow(net))
})

test_that("causnet() does not depend on the random number generator state", {
  d <- triangle_data()
  nets <- lapply(1:5, function(s) {
    set.seed(s)
    causnet(d, alpha = 0.05, max_parents = 2)$network
  })
  for (i in 2:5) expect_identical(nets[[i]], nets[[1]])
})
