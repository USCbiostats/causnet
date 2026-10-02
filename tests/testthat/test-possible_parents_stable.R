test_that("find_possible_parents_stable() returns one element per variable", {
  set.seed(1)
  d <- simdat(sample.size = 200, n_var = 5, beta = 0.6)
  d$V6 <- rnorm(200)
  d$V7 <- rnorm(200)  # trailing noise nodes without stable parents

  pp <- suppressMessages(
    find_possible_parents_stable(d, tau = 1, pi_thr = 0.75, B = 50, seed = 1)
  )

  expect_length(pp, 7)
  expect_null(pp[[7]])
  # same representation as find_possible_parents(): NULL or integer indices
  expect_true(all(vapply(pp, function(x) is.null(x) || is.integer(x),
                         logical(1))))
})

test_that("causnet_stable() can use a last node that has no stable parents", {
  set.seed(1)
  n <- 200
  d <- data.frame(V1 = rnorm(n))
  d$V2 <- 0.6 * d$V1 + rnorm(n)
  d$V5 <- rnorm(n)
  d$V4 <- 0.8 * d$V5 + rnorm(n)
  d$V3 <- 0.8 * d$V5 + rnorm(n)
  d <- d[, c("V1", "V2", "V3", "V4", "V5")]  # V5 -> V3, V5 -> V4

  net <- suppressMessages(
    causnet_stable(d, tau = 1, pi_thr = 0.75, B = 50, seed = 1)
  )$network
  edges <- paste(net$from, net$to)
  expect_true(all(c("5 3", "5 4") %in% edges))

  pp <- suppressMessages(
    find_possible_parents_stable(d, tau = 1, pi_thr = 0.75, B = 50, seed = 1)
  )
  expect_length(pp, 5)
  expect_null(pp[[5]])
})

test_that("find_possible_parents_stable() rejects tau that is not a positive finite number", {
  set.seed(1)
  d <- simdat(sample.size = 50, n_var = 4)
  for (tau in list(0, -1, Inf, NA_real_, NA, "1", c(1, 2))) {
    expect_error(find_possible_parents_stable(d, tau = tau, B = 2),
                 "`tau` must be a positive finite number")
  }
  expect_error(suppressMessages(find_possible_parents_stable(d, tau = 0.5, B = 2)),
               NA)
  # causnet_stable() applies the same check
  for (tau in list(0, -1, Inf, NA_real_, NA, "1", c(1, 2))) {
    expect_error(causnet_stable(d, tau = tau, B = 2),
                 "`tau` must be a positive finite number")
  }
})

test_that("find_possible_parents_stable() checks its arguments before setting the seed", {
  set.seed(1)
  d <- simdat(sample.size = 50, n_var = 4)
  set.seed(2)
  state <- get(".Random.seed", envir = globalenv())
  expect_error(find_possible_parents_stable(d, tau = -1, B = 2, seed = 3),
               "`tau` must be")
  expect_error(find_possible_parents_stable(d, pi_thr = 0.4, B = 2, seed = 3),
               "`pi_thr` must be")
  expect_true(identical(get(".Random.seed", envir = globalenv()), state))
})
