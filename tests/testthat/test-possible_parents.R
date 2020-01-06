test_that("find_possible_parents works", {
  new_data <- simdat(n_var = 5)

  pp <- find_possible_parents(new_data, 0.05)

  expect_length(pp, 5)
  expect_equal(sapply(pp, is.integer), rep(TRUE, 5))

  # Edge cases
  expect_equal(sapply(find_possible_parents(new_data, 1), length), rep(4, 5))
  expect_equal(sapply(find_possible_parents(new_data, 0), length), rep(0, 5))
})
