logical_ind <- function(n, i) {
  res <- logical(n)
  res[i] <- TRUE
  res
}

test_that("list_setequal works", {

  x <- list(1, 2:5, 3:4, 4, 5:7, 6)

  expect_equal(
    list_setequal(x, 1),
    logical_ind(6, 1)
  )

  expect_equal(
    list_setequal(x, 3:4),
    logical_ind(6, 3)
  )
})

test_that("list_setequal only returns first result", {

  x <- list(1, 2, 3)

  expect_equal(
    list_setequal(x, 4),
    logical_ind(3, NA)
  )

  expect_equal(
    list_setequal(x, c(1, 1)),
    logical_ind(3, NA)
  )

  expect_equal(
    list_setequal(x, c(1, 2)),
    logical_ind(3, NA)
  )

  expect_equal(
    list_setequal(x, integer()),
    logical_ind(3, NA)
  )
})

test_that("list_setequal only returns first result", {

  x <- list(1, 1, 2)

  expect_equal(
    list_setequal(x, 1),
    logical_ind(3, 1)
  )
})

test_that("list_setequal cares about order", {

  x <- list(c(1, 4))

  expect_equal(
    list_setequal(x, c(4, 1)),
    logical_ind(1, NA)
  )

  expect_equal(
    list_setequal(x, c(1, 4)),
    logical_ind(1, 1)
  )
})


