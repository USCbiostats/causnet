test_that("multiplication works", {
  expect_equal(subsetr(4, numeric()), 0)
  expect_equal(subsetr(4, c(4)), 1)
  expect_equal(subsetr(4, c(1)), 2^(4-1))
  expect_equal(subsetr(4, c(1, 2, 3, 4)), 2^4-1)
})
