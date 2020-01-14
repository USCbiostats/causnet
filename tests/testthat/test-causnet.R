set.seed(1234)
test_that("Input validation for causnet", {
  new_data <- simdat(n_var = 5)

  # alpha
  expect_error(causnet(data = new_data, 0.1), NA)
  expect_error(causnet(data = new_data, "0.5"), "`alpha` must")
  expect_error(causnet(data = new_data, c(0.2, 0.5)), "`alpha` must")
  expect_error(causnet(data = new_data, -0.01), "`alpha` must")
  expect_error(causnet(data = new_data, 1.01), "`alpha` must")
})
