# Sink lists preallocate k / sink / wscore to m^4 rows, but can grow past that.
# These tests use m = 2 (capacity 16) and push 16-20 rows into the list.

overflow_rows <- function() {
  ids <- c(1, 2, 1, 2, 5:20)  # rows 3 and 4 duplicate rows 1 and 2
  k <- c(rep(2, 17), rep(3, 3))
  windx <- lapply(seq_along(ids), function(r) ids[r] * 10 + seq_len(k[r]))
  list(windx = windx, k = k, sink = vapply(windx, max, numeric(1)),
       wscore = -seq_along(ids))
}

overflowed_deduplicated_list <- function() {
  x <- overflow_rows()
  sl <- create_sink_list(list(), numeric(), numeric(), numeric(), 2)
  sl <- append_sink_list(sl, x$windx, x$k, x$sink, x$wscore)
  remove_duplicates(sl)
}

test_that("remove_duplicates() keeps every row after the list outgrew m^4", {
  x <- overflow_rows()
  sl <- overflowed_deduplicated_list()
  keep <- c(1, 2, 5:20)
  n <- attr(sl, "index")

  expect_equal(n, 18)
  expect_equal(sl$k[seq_len(n)], x$k[keep])
  expect_equal(sl$sink[seq_len(n)], x$sink[keep])
  expect_equal(sl$wscore[seq_len(n)], x$wscore[keep])
  expect_true(all(sl$k[-seq_len(n)] == 0))
})

test_that("set_sink_list() keeps every row when given more than m^4 rows", {
  x <- overflow_rows()
  for (n in c(16, 18)) {
    sl <- create_sink_list(list(), numeric(), numeric(), numeric(), 2)
    sl <- set_sink_list(sl, x$windx[seq_len(n)], x$k[seq_len(n)],
                        x$sink[seq_len(n)], x$wscore[seq_len(n)])
    expect_equal(attr(sl, "index"), n)
    expect_equal(sl$k[seq_len(n)], x$k[seq_len(n)])
    expect_equal(sl$wscore[seq_len(n)], x$wscore[seq_len(n)])
    expect_true(all(sl$k[-seq_len(n)] == 0))
  }
})

test_that("set_sink_list() below capacity keeps the preallocated layout", {
  x <- overflow_rows()
  sl <- create_sink_list(list(), numeric(), numeric(), numeric(), 2)
  sl <- set_sink_list(sl, x$windx[1:12], x$k[1:12], x$sink[1:12],
                      x$wscore[1:12])
  expect_equal(sl$k, c(x$k[1:12], rep(0, 4)))
  expect_equal(sl$wscore, c(x$wscore[1:12], rep(0, 4)))
})

test_that("find_best_network() runs on a list that outgrew m^4", {
  sl <- overflowed_deduplicated_list()
  expect_error(bn <- find_best_network(sl), NA)
  expect_true(all(bn$k >= 2))
  expect_equal(lengths(bn$windx), bn$k)
})

test_that("find_best_network() fails loudly on stale rows beyond `index`", {
  sl <- create_sink_list(list(), numeric(), numeric(), numeric(), 2)
  sl <- append_sink_list(sl, list(c(1, 2), c(2, 3)), c(2, 2), c(2, 3),
                         c(-1, -2))
  sl$k[5] <- 3  # stale row beyond the 2 valid rows
  expect_error(find_best_network(sl), "beyond the 2 valid rows")
})
