test_that("swscore respects max_parents", {
 expect_equal(
   swscore(1,
           c(1, 2, 3),
           list(c(1, 2, 3)),
           list(c(4,5), c(6, 7), c(8, 9)),
           list(c(4,5), c(6, 7), c(8, 9)),
           2),
   list(Inf, Inf)
 )

 expect_equal(
   swscore(1,
           c(1, 2, 3),
           list(c(1, 2, 3)),
           list(list(c(4, 5), c(1, 2, 3))),
           list(list(list(1, 2)), list(list(3, 4))),
           4),
   list(2, 4)
 )
})

test_that("Makes sure wsink_scores removes cases that violate max_parents", {

  ref <- list(w = c(2L, 3L, 5L),
              w_networkscore = -1281.63810094545,
              pp = list(c(2L, 5L),
                        c(1L, 3L, 4L),
                        c(2L, 4L, 5L),
                        c(2L, 3L, 5L),
                        c(1L, 3L, 4L)),
              po = list(c(2L, 5L),
                        c(1L, 3L, 4L),
                        c(2L, 4L, 5L),
                        c(2L, 3L, 5L),
                        c(1L, 3L, 4L)),
              pps = list(list(2L, 5L, c(2L, 5L)),
                         list(1L, 3L, 4L, c(1L, 3L), c(1L, 4L), 3:4),
                         list(2L, 4L, 5L, c(2L, 4L), c(2L, 5L), 4:5),
                         list(2L, 3L, 5L, 2:3, c(2L, 5L), c(3L, 5L)),
                         list(1L, 3L, 4L, c(1L, 3L), c(1L, 4L), 3:4)),
              bps = list(list(list(2L, NULL, 2L),
                              list(1L, 3L, NULL, c(1L, 3L), 1L, 3L),
                              list(2L, 4L, 5L, c(2L, 4L), c(2L, 5L), 4L),
                              list(NULL, 3L, 5L, 3L, c(2L, 5L), c(3L, 5L)),
                              list(NULL, 3L, 4L, 3L, 4L, 4L)),
                         list(list(-407.1, -430.8, -407.1),
                              list(-407.1, -420.0, -430.8, -392.8, -407.1, -420.0),
                              list(-420.0, -421.4, -430.6, -412.9, -419.6, -421.4),
                              list(-430.8, -421.4, -410.6, -421.4, -410.4, -404.0),
                              list(-430.8, -430.6, -410.6, -430.6, -410.6, -410.6))),
              m = 5L,
              max_parents = 2)

  result <- wsink_scores(ref$w,
                         ref$w_networkscore,
                         ref$pp,
                         ref$po,
                         ref$pps,
                         ref$bps,
                         ref$m,
                         ref$max_parents)

  expect_false(
    any(is.infinite(result$wscore))
  )
})
