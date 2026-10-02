# Descending loops "from:2" count upwards when from == 1 (1:2 is c(1, 2)).

test_that("seq_down_to_2() counts down and is empty below 2", {
  expect_identical(seq_down_to_2(4L), 4:2)
  expect_identical(seq_down_to_2(2L), 2L)
  expect_identical(seq_down_to_2(1L), integer())
  expect_identical(seq_down_to_2(0L), integer())
})

test_that("find_best_network() does not walk down from a single node", {
  # The largest set {1, 2} leaves one node after removing its sink, so there
  # is no smaller set to walk to; the one-node row must not be picked up.
  bs <- create_sink_list(list(), numeric(), numeric(), numeric(), 2)
  bs <- append_sink_list(bs, list(c(1, 2), 1), c(2, 1), c(2, 1), c(-1, -2))

  bn <- find_best_network(bs)

  expect_equal(bn$k, 2)
  expect_equal(bn$windx, list(c(1, 2)))
})
