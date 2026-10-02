# Helpers shared by the test files.

has_two_cycle <- function(net) {
  any(paste(net$from, net$to) %in% paste(net$to, net$from))
}

is_dag <- function(net) {
  e <- unique(net[, c("from", "to")])
  while (nrow(e) > 0) {
    roots <- setdiff(e$from, e$to)
    if (length(roots) == 0) return(FALSE)
    e <- e[!e$from %in% roots, , drop = FALSE]
  }
  TRUE
}

# Three pairwise dependent variables (V1 -> V2, V1 -> V3, V2 -> V3); the best
# network is a complete DAG in which one node has two parents.
triangle_data <- function() {
  set.seed(3)
  n <- 300
  d <- data.frame(V1 = rnorm(n))
  d$V2 <- 0.5 * d$V1 + rnorm(n)
  d$V3 <- 0.5 * d$V1 + 0.5 * d$V2 + rnorm(n)
  d
}

# Linear Gaussian data from a random DAG in which every edge runs from a lower
# to a higher column index. About half of the nodes get no parents, so many
# columns are independent of all others.
sparse_dag_data <- function(p, n = 300, seed = 1, coef = 0.5) {
  set.seed(seed)
  x <- matrix(stats::rnorm(n * p), n, p)
  for (i in 2:p) {
    k <- sample(0:2, 1, prob = c(0.5, 0.25, 0.25))
    for (pa in sample.int(i - 1, min(k, i - 1))) {
      x[, i] <- x[, i] + coef * x[, pa]
    }
  }
  colnames(x) <- paste0("V", seq_len(p))
  as.data.frame(x)
}

# Total score (score_bic_lm) of a network: every column of `d` with its
# parents in `net` (columns `from` / `to`), or without parents.
network_total_score <- function(d, net) {
  sum(vapply(seq_len(ncol(d)), function(v) {
    pa <- sort(net$from[net$to == v])
    score_bic_lm(v, if (length(pa) > 0) pa else NA, d)
  }, numeric(1)))
}

# All orderings of the elements of x.
permutations <- function(x) {
  if (length(x) <= 1) return(list(x))
  unlist(lapply(seq_along(x), function(i) {
    lapply(permutations(x[-i]), function(rest) c(x[i], rest))
  }), recursive = FALSE)
}

# Brute force (small p only): best total score (score_bic_lm) over complete
# generational orderings of the nodes that have candidate parents or are
# candidate parents in `pp`; the other nodes add their score without parents.
# In a complete generational ordering every node after the first has a
# candidate parent before it. Each node takes its best parent set among its
# candidate parents before it, with at most `max_parents` members (the empty
# set included). -Inf if no complete generational ordering exists.
best_generational_score <- function(d, pp, max_parents = 2) {
  local_score <- function(v, pa) {
    score_bic_lm(v, if (length(pa) > 0) pa else NA, d)
  }
  nodes <- sort(unique(c(which(lengths(pp) > 0), unlist(pp))))
  sets <- lapply(seq_len(ncol(d)), function(v) {
    cand <- pp[[v]]
    out <- list(integer())
    for (k in seq_len(min(length(cand), max_parents))) {
      idx <- utils::combn(length(cand), k, simplify = FALSE)
      out <- c(out, lapply(idx, function(i) cand[i]))
    }
    out
  })
  scores <- lapply(seq_len(ncol(d)), function(v) {
    vapply(sets[[v]], function(pa) local_score(v, pa), numeric(1))
  })
  rest <- sum(vapply(setdiff(seq_len(ncol(d)), nodes),
                     function(v) local_score(v, integer()), numeric(1)))

  best <- -Inf
  for (ord in permutations(nodes)) {
    total <- 0
    for (i in seq_along(ord)) {
      v <- ord[i]
      before <- ord[seq_len(i - 1)]
      if (i > 1 && !any(is.element(pp[[v]], before))) {
        total <- -Inf
        break
      }
      ok <- vapply(sets[[v]], function(pa) all(is.element(pa, before)),
                   logical(1))
      total <- total + max(scores[[v]][ok])
    }
    best <- max(best, total)
  }
  best + rest
}
