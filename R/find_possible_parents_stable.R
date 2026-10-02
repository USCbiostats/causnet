#' Stability Selection for Parent Identification
#'
#' A stability-selection variant of \code{find_possible_parents} that
#' replaces the single p-value threshold with subsampling-based selection
#' frequencies, following Meinshausen & Buhlmann (2010). The number of
#' variables selected per subsample is derived from a per-node bound on the
#' per-family error rate (PFER, see \code{tau}).
#'
#' @param data A data matrix or data frame (n x p).
#' @param tau Per-node PFER bound, under the assumptions of Theorem 1 of
#'   Meinshausen & Buhlmann (2010) (see Details): the expected number of
#'   falsely selected candidate parents of each node (each node is screened
#'   as its own family of \eqn{p-1} candidates). Summed over the screened
#'   nodes, the expected total number of falsely selected candidate parents
#'   (not edges of the final network) is at most (number of screened
#'   nodes) x \code{tau}. If the formula for \code{q} (see Details) gives 0
#'   (very small \code{tau} or very few variables), \code{q} is set to 1,
#'   and the per-node bound is then larger than \code{tau}. Must be a
#'   positive finite number. Default 1.
#' @param pi_thr Stability threshold. Must be in (0.5, 1). Default 0.75.
#' @param B Number of subsamples. Default 100.
#' @param seed Optional random seed.
#'
#' @return A list of length \code{ncol(data)}; element \code{i} contains
#'   the indices of stable parent candidates for node \code{i}, or
#'   \code{NULL} if node \code{i} has none.
#'
#' @details
#' For each node \eqn{V_i}, the function:
#' \enumerate{
#'   \item Computes \code{q} from Theorem 1 of Meinshausen & Buhlmann (2010):
#'     \deqn{q = \lfloor \sqrt{\tau \cdot (p-1) \cdot (2 \pi_{thr} - 1)} \rfloor}
#'   \item For \code{B} subsamples of size \eqn{\lfloor n/2 \rfloor}, drawn
#'     without replacement, ranks all other nodes by absolute Pearson
#'     correlation with \eqn{V_i} and retains the top \code{q}.
#'   \item Computes the empirical selection frequency \eqn{\hat\Pi_j^{(i)}}
#'     for each candidate \eqn{V_j}.
#'   \item Returns \eqn{\{j : \hat\Pi_j^{(i)} \geq \pi_{thr}\}}.
#' }
#' The bound on the PFER holds under the assumptions of that theorem
#' (exchangeable selection of the noise variables, and a selection procedure
#' that is not worse than random guessing).
#'
#' @references
#' Meinshausen, N. and Buhlmann, P. (2010). Stability selection.
#' Journal of the Royal Statistical Society: Series B, 72(4), 417-473.
#'
#' @export
find_possible_parents_stable <- function(data,
                                         tau = 1,
                                         pi_thr = 0.75,
                                         B = 100,
                                         seed = NULL) {

  if (pi_thr <= 0.5 || pi_thr >= 1) {
    stop("`pi_thr` must be in (0.5, 1).", call. = FALSE)
  }
  if (!is.numeric(tau) || length(tau) != 1 || !is.finite(tau) || tau <= 0) {
    stop("`tau` must be a positive finite number.", call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)

  n     <- nrow(data)
  n_var <- ncol(data)
  n_sub <- floor(n / 2)

  # ---- Step 0: derive q from Theorem 1 ----
  p_candidate <- n_var - 1
  q <- floor(sqrt(tau * p_candidate * (2 * pi_thr - 1)))
  q <- max(q, 1)

  # ---- Step 1: B subsamples, accumulate selection counts ----
  selection_count <- matrix(0, nrow = n_var, ncol = n_var)

  for (b in seq_len(B)) {
    I_b <- sample.int(n, size = n_sub, replace = FALSE)
    data_sub <- data[I_b, , drop = FALSE]

    for (vertex in seq_len(n_var)) {
      scores <- numeric(n_var)
      for (parent in seq_len(n_var)) {
        if (parent != vertex) {
          scores[parent] <- abs(stats::cor(
            data_sub[, vertex, drop = TRUE],
            data_sub[, parent, drop = TRUE]
          ))
        } else {
          scores[parent] <- -Inf
        }
      }
      top_q <- order(scores, decreasing = TRUE)[seq_len(q)]
      selection_count[vertex, top_q] <-
        selection_count[vertex, top_q] + 1
    }
  }

  # ---- Step 2: empirical selection frequencies ----
  Pi <- selection_count / B

  # ---- Step 3: apply stability threshold ----
  # nodes without stable parents stay NULL, as in find_possible_parents()
  # (assigning NULL with `[[<-` would delete the element and shorten the list)
  out <- vector("list", n_var)
  for (vertex in seq_len(n_var)) {
    parents <- which(Pi[vertex, ] >= pi_thr)
    if (length(parents) > 0) out[[vertex]] <- parents
  }

  message(sprintf(
    "[stability selection] n=%d, p=%d, q=%d, pi_thr=%.2f, tau=%.2f, B=%d",
    n, n_var, q, pi_thr, tau, B
  ))

  out
}
