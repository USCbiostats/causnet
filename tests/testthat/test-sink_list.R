library(causnet)

data_list <- create_sink_list(list(), numeric(), numeric(), numeric(), 5)

data_list <- append_sink_list(data_list,
  windx = list(1:2, 2:1, 2:3, c(2L, 4L), 3:2, 3:4,
               c(4L, 2L), 4:3, 4:5, 5:4),
  k = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
  sink = c(2, 1, 3, 4, 2, 4, 2, 3, 5, 4),
  wscore = c(-854.85393060559,
             -854.85393060559, -844.238690128917, -855.182188993158,
             -844.238690128917, -856.876146916413, -855.182188993157,
             -856.876146916413, -856.964434738934, -856.964434738934)
)

test_that("remove_duplicates works as intended", {

  expect_equal(
   as.list(remove_duplicates(data_list)),
  list(
       sink = c(2, 1, 3, 4, 2, 4, 2, 3, 5, 4,
                rep(0, attr(data_list, "length") - 10)),
       windx = list(1:2, 2:1, 2:3, c(2L, 4L), 3:2, 3:4,
                    c(4L, 2L), 4:3, 4:5, 5:4),
       wscore = c(-854.85393060559,
                  -854.85393060559, -844.238690128917, -855.182188993158,
                  -844.238690128917, -856.876146916413, -855.182188993157,
                  -856.876146916413, -856.964434738934, -856.964434738934,
                  rep(0, attr(data_list, "length") - 10)),
       k = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
             rep(0, attr(data_list, "length") - 10))
       )
  )
})
