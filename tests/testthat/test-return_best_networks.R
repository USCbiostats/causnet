# return_all_best_networks() and return_random_best_networks() are deprecated:
# every call gives a warning, and the value is the same as before the
# deprecation. The expected networks below are the values that the package
# returned just before the deprecation, on the triangle_data() example
# (helper-graph.R), where the best parent set of node 3 is {1, 2}.

# one of the enumerated networks: node 1 -> 2 plus ONE parent of node 3
one_parent_net <- function(parent_of_3) {
  structure(list(node.source = c(parent_of_3, 1), node.sink = c(3, 2),
                 component = c(1, 1)),
            row.names = 1:2, class = "data.frame")
}

deprecation_msg <- paste(
  "is deprecated. It enumerates networks that take one parent from the",
  "single best parent set of each sink; these are not genuinely tied best",
  "networks"
)

test_that("return_all_best_networks() warns on every call and returns the same networks", {
  out <- causnet(triangle_data(), alpha = 0.05, max_parents = 2)

  expect_warning(nets <- return_all_best_networks(out), deprecation_msg)
  expect_identical(nets, list(one_parent_net(1), one_parent_net(2)))
  # the warning is given again on the next call
  expect_warning(return_all_best_networks(out), "is deprecated")
})

test_that("return_random_best_networks() warns on every call and returns the same networks", {
  out <- causnet(triangle_data(), alpha = 0.05, max_parents = 2)

  set.seed(99)
  expect_warning(nets <- return_random_best_networks(out, 1), deprecation_msg)
  expect_identical(nets, list(one_parent_net(2)))

  set.seed(99)
  expect_warning(nets <- return_random_best_networks(out, 2), "is deprecated")
  expect_identical(nets, list(one_parent_net(2), one_parent_net(1)))

  expect_output(
    expect_warning(nets <- return_random_best_networks(out, 3), "is deprecated"),
    "Returning 2 best networks"
  )
  expect_identical(nets, list(one_parent_net(1), one_parent_net(2)))
})

test_that("the deprecated functions still return an empty list for an empty network", {
  out <- causnet(sparse_dag_data(6, seed = 1), alpha = 0)

  expect_warning(nets <- return_all_best_networks(out), "is deprecated")
  expect_identical(nets, list())
  expect_output(
    expect_warning(nets <- return_random_best_networks(out, 1), "is deprecated"),
    "Returning 0 best networks"
  )
  expect_identical(nets, list())
})
