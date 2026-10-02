# max_wscore() groups the rows of a sink list by node set and keeps the best
# rows of each group.

test_that("max_wscore() keeps node sets apart whose indices would run together", {
  # {1, 112} and {11, 12} both gave the key "1112" without a separator
  sinks <- list(windx = list(c(1, 112), c(112, 1), c(11, 12), c(12, 11)),
                wscore = c(-10, -12, -20, -5))

  expect_identical(max_wscore(sinks), c(1L, 4L))
})

test_that("max_wscore() keeps the order of the groups without key collisions", {
  # the function as it was before the separator was added
  max_wscore_no_separator <- function(sinks_tmp) {
    new_vals <- split(
      seq_along(sinks_tmp$windx),
      purrr::map_chr(sinks_tmp$windx, ~ paste(sort(.x), collapse = ""))
    )
    unique(unlist(lapply(new_vals, give_back_max, sinks_tmp$wscore)))
  }
  # sets with two-digit indices, and ties
  sinks <- list(
    windx = list(c(9, 12), c(10, 11), c(12, 9), c(1, 2), c(11, 10), c(2, 1),
                 c(2, 3, 10), c(3, 10, 2), c(10, 2, 3), c(4, 5)),
    wscore = c(-3, -7, -3, -1, -8, -2, -5, -4, -4, -6)
  )

  expect_identical(max_wscore(sinks), max_wscore_no_separator(sinks))
  # groups in the order "1011", "12", "2310", "45", "912"
  expect_identical(max_wscore(sinks), c(2L, 4L, 8L, 9L, 10L, 1L, 3L))
})
