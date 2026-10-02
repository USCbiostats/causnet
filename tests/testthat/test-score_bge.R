bge_test_data <- function() {
  set.seed(7)
  n <- 200
  d <- data.frame(V1 = rnorm(n))
  d$V2 <- 0.6 * d$V1 + rnorm(n)
  d$V3 <- rnorm(n)
  d$V4 <- 0.5 * d$V2 + 0.5 * d$V3 + rnorm(n)
  d
}

test_that("score_bge() local scores add up to the bnlearn BGe network score", {
  d <- bge_test_data()
  parents <- list(NA, 1L, NA, c(2L, 3L))
  dag <- bnlearn::model2network("[V1][V2|V1][V3][V4|V2:V3]")

  local <- vapply(1:4, function(j) score_bge(j, parents[[j]], d), numeric(1))

  expect_equal(sum(local), bnlearn::score(dag, d, type = "bge"))
  expect_equal(local,
               unname(bnlearn::score(dag, d, type = "bge", by.node = TRUE)[names(d)]))
})

test_that("score_bge() is bigger-is-better: the true parent wins", {
  d <- bge_test_data()

  expect_gt(score_bge(2, 1L, d), score_bge(2, 3L, d))
  expect_gt(score_bge(2, 1L, d), score_bge(2, NA, d))
  expect_gt(score_bge(4, c(2L, 3L), d), score_bge(4, c(1L, 3L), d))
})
