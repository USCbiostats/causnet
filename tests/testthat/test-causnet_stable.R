test_that("causnet_stable() is deterministic given seed", {
  d <- sparse_dag_data(9, seed = 3)

  a <- suppressMessages(causnet_stable(d, tau = 2, B = 30, seed = 11))
  set.seed(99)
  stats::runif(3)
  b <- suppressMessages(causnet_stable(d, tau = 2, B = 30, seed = 11))
  expect_identical(b$network, a$network)
  expect_identical(b$n_best_parents, a$n_best_parents)

  a <- suppressMessages(
    causnet_stable(d, tau = 2, B = 30, seed = 11, outcome = 9, n_levels = 2)
  )
  set.seed(7)
  b <- suppressMessages(
    causnet_stable(d, tau = 2, B = 30, seed = 11, outcome = 9, n_levels = 2)
  )
  expect_identical(b$network, a$network)
})

test_that("causnet() and causnet_stable() return DAGs, also with an outcome", {
  for (seed in 1:6) {
    d <- sparse_dag_data(12, seed = seed)
    oc <- 12 - seed %% 3
    nets <- list(
      causnet(d, alpha = 0.01)$network,
      causnet(d, alpha = 0.01, outcome = oc, n_levels = 2)$network,
      suppressMessages(causnet_stable(d, tau = 2, B = 30, seed = seed))$network,
      suppressMessages(causnet_stable(d, tau = 2, B = 30, seed = seed,
                                      outcome = oc, n_levels = 3))$network
    )
    for (net in nets) {
      expect_false(has_two_cycle(net))
      expect_true(is_dag(net))
      expect_equal(anyDuplicated(net[, c("from", "to")]), 0)
    }
  }
})
