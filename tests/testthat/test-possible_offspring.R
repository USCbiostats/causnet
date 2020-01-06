test_that("multiplication works", {
  pp <- list(2:3, 3, 1)

  expect_equal(
    find_possible_offspring(pp),
    list(c(3), c(1), c(1, 2))
  )
})
