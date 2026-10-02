# Phenotype-driven search: candidate parents are restricted to the nodes
# within `n_levels` hops of the outcome.

# candidate parents of an undirected chain 1 - 2 - ... - p, as
# find_possible_parents() returns them for chain data
chain_pp <- function(p) {
  lapply(seq_len(p), function(v) {
    x <- c(v - 1L, v + 1L)
    x[x >= 1 & x <= p]
  })
}

test_that("pheno_expand() reaches at most n_levels hops from the outcome", {
  ex <- pheno_expand(chain_pp(7), outcome = 7L, n_levels = 2)

  # reached: 7 (outcome), 6 (level 1), 5 (level 2); node 5 is on the deepest
  # level and keeps only its reached candidate parent 6 (not 4)
  expect_identical(ex, list(NULL, NULL, NULL, NULL, 6L, c(5L, 7L), 6L))
})

test_that("pheno_expand() keeps everything once all nodes are reached", {
  expect_identical(pheno_expand(chain_pp(7), outcome = 4L, n_levels = 3),
                   chain_pp(7))
  expect_identical(pheno_expand(chain_pp(7), outcome = 7L, n_levels = 10),
                   chain_pp(7))
})

test_that("pheno_expand() follows candidate parents, not candidate offspring", {
  pp <- list(NULL, 1L, 2L, NULL)  # 1 -> 2 -> 3, node 4 unconnected
  expect_identical(pheno_expand(pp, outcome = 2L, n_levels = 3),
                   list(NULL, 1L, NULL, NULL))
})

test_that("an outcome whose neighbourhood covers all nodes gives the whole-graph network", {
  set.seed(1)
  d <- simdat(sample.size = 300, n_var = 6, beta = 0.6)  # chain 1 -> ... -> 6
  pp <- find_possible_parents(d, 0.01)
  expect_identical(pheno_expand(pp, 6L, 5), pp)

  whole <- causnet(d, alpha = 0.01)
  expect_identical(causnet(d, alpha = 0.01, outcome = NULL)$network,
                   whole$network)
  expect_identical(causnet(d, alpha = 0.01, outcome = 6, n_levels = 5)$network,
                   whole$network)
})

test_that("phenotype-driven causnet() only uses the reached nodes", {
  d <- sparse_dag_data(12, seed = 3)
  pp <- find_possible_parents(d, 0.01)
  ex <- pheno_expand(pp, 12L, 1)
  reached <- unique(c(which(!vapply(ex, is.null, logical(1))), unlist(ex)))
  expect_lt(length(reached), 12)

  out <- causnet(d, alpha = 0.01, outcome = 12, n_levels = 1)
  expect_gt(nrow(out$network), 0)
  expect_true(all(c(out$network$from, out$network$to) %in% reached))
  expect_identical(causnet(d, alpha = 0.01, outcome = "V12", n_levels = 1)$network,
                   out$network)

  st <- suppressMessages(
    causnet_stable(d, tau = 2, B = 30, seed = 1, outcome = 12, n_levels = 1)
  )
  pp_st <- suppressMessages(
    find_possible_parents_stable(d, tau = 2, B = 30, seed = 1)
  )
  ex_st <- pheno_expand(pp_st, 12L, 1)
  reached_st <- unique(c(which(!vapply(ex_st, is.null, logical(1))),
                         unlist(ex_st)))
  expect_true(all(c(st$network$from, st$network$to) %in% reached_st))
  st_name <- suppressMessages(
    causnet_stable(d, tau = 2, B = 30, seed = 1, outcome = "V12", n_levels = 1)
  )
  expect_identical(st_name$network, st$network)
})

test_that("invalid outcome and n_levels are rejected", {
  d <- sparse_dag_data(5, seed = 1)
  expect_error(causnet(d, outcome = "V9"), "`outcome` must")
  expect_error(causnet(d, outcome = 0), "`outcome` must")
  expect_error(causnet(d, outcome = c(1, 2)), "`outcome` must")
  expect_error(causnet(d, outcome = TRUE), "`outcome` must")
  expect_error(causnet(d, outcome = 5, n_levels = 0), "`n_levels` must")
  expect_error(causnet(d, outcome = 5, n_levels = 1.5), "`n_levels` must")
  expect_error(causnet(d, outcome = 5, n_levels = Inf), "`n_levels` must")
  expect_error(causnet_stable(d, outcome = "V9"), "`outcome` must")
  expect_error(causnet_stable(d, outcome = 5, n_levels = -1), "`n_levels` must")
  # n_levels is checked even when outcome is NULL (documented)
  expect_error(causnet(d, n_levels = 1.5), "`n_levels` must")
  expect_error(causnet_stable(d, n_levels = NA), "`n_levels` must")
})
