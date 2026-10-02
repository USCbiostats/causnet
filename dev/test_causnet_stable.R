# =============================================================================
# Test script for causnet_stable()
# Author: Tianjiao Cao
# Purpose: Comprehensive comparison of causnet() vs causnet_stable()
#
# Sections:
#   1. Setup
#   2. Demo 1: Single-run comparison at p=10
#   3. Demo 2: Parameter sensitivity (single-run)
#   4. Demo 3: Robustness probe (varying p)
#   5. Demo 4: Repeated experiments (50 runs)
# =============================================================================


# ---------- 1. Setup ----------

devtools::load_all(".")

# Helper: count true positives, false positives, false negatives
# Treats edges as undirected (BN equivalence allows direction reversals)
evaluate_network <- function(edges_df, true_adjacencies) {
  if (is.null(edges_df) || nrow(edges_df) == 0) {
    return(c(TP = 0, FP = 0, FN = length(true_adjacencies)))
  }

  # Convert edges to canonical undirected form: "min-max"
  pred_edges <- apply(edges_df[, c("from", "to")], 1, function(e) {
    paste(sort(e), collapse = "-")
  })
  pred_edges <- unique(pred_edges)

  TP <- sum(pred_edges %in% true_adjacencies)
  FP <- sum(!(pred_edges %in% true_adjacencies))
  FN <- sum(!(true_adjacencies %in% pred_edges))

  c(TP = TP, FP = FP, FN = FN)
}

# Helper: build true adjacencies for a chain network 1-2-3-...-p
build_chain_adjacencies <- function(p) {
  sapply(1:(p - 1), function(i) paste(i, i + 1, sep = "-"))
}

# Helper: extract component-1 edges (CausNet's first network candidate)
get_component1_edges <- function(result) {
  if (is.null(result$network)) return(NULL)
  result$network[result$network$component == 1, c("from", "to"), drop = FALSE]
}

# Helper: pretty section header
section <- function(title) {
  cat("\n")
  cat(paste0(rep("=", 70), collapse = ""), "\n")
  cat("  ", title, "\n")
  cat(paste0(rep("=", 70), collapse = ""), "\n")
}


# ---------- 2. Demo 1: Single-run comparison at p=10 ----------

section("Demo 1: Single-run comparison (p=10, n=300)")

set.seed(1234)
mydata_10 <- simdat(n_var = 10)
true_adj_10 <- build_chain_adjacencies(10)

# Original
res_orig_10 <- causnet(mydata_10, alpha = 0.05)
edges_orig_10 <- get_component1_edges(res_orig_10)
metrics_orig_10 <- evaluate_network(edges_orig_10, true_adj_10)

# Stable
res_stable_10 <- causnet_stable(
  mydata_10, tau = 1, pi_thr = 0.75, B = 100, seed = 1234
)
edges_stable_10 <- get_component1_edges(res_stable_10)
metrics_stable_10 <- evaluate_network(edges_stable_10, true_adj_10)

cat("\nOriginal causnet (alpha=0.05):\n")
cat(sprintf("  Component 1 edges: %d\n", nrow(edges_orig_10)))
cat(sprintf("  TP=%d, FP=%d, FN=%d\n",
            metrics_orig_10["TP"], metrics_orig_10["FP"], metrics_orig_10["FN"]))

cat("\ncausnet_stable (tau=1, pi_thr=0.75):\n")
cat(sprintf("  Component 1 edges: %d\n", nrow(edges_stable_10)))
cat(sprintf("  TP=%d, FP=%d, FN=%d\n",
            metrics_stable_10["TP"], metrics_stable_10["FP"], metrics_stable_10["FN"]))


# ---------- 3. Demo 2: Parameter sensitivity (single-run, p=10) ----------

section("Demo 2: Parameter sensitivity (single-run, p=10)")

param_grid <- expand.grid(
  tau = c(0.5, 1, 2, 5),
  pi_thr = c(0.6, 0.75, 0.9)
)

sensitivity_results <- data.frame()
for (i in seq_len(nrow(param_grid))) {
  tau_i <- param_grid$tau[i]
  pi_i  <- param_grid$pi_thr[i]

  # Skip invalid combinations (numerical issues at extreme parameters)
  res <- tryCatch(
    causnet_stable(mydata_10, tau = tau_i, pi_thr = pi_i,
                   B = 100, seed = 1234),
    error = function(e) NULL
  )

  if (is.null(res)) {
    sensitivity_results <- rbind(sensitivity_results, data.frame(
      tau = tau_i, pi_thr = pi_i,
      n_edges = NA, n_components = NA, TP = NA, FP = NA, FN = NA,
      status = "FAILED"
    ))
  } else {
    edges <- get_component1_edges(res)
    m <- evaluate_network(edges, true_adj_10)
    sensitivity_results <- rbind(sensitivity_results, data.frame(
      tau = tau_i, pi_thr = pi_i,
      n_edges = nrow(res$network),
      n_components = max(res$network$component),
      TP = m["TP"], FP = m["FP"], FN = m["FN"],
      status = "OK"
    ))
  }
}

cat("\n")
print(sensitivity_results, row.names = FALSE)


# ---------- 4. Demo 3: Robustness probe (varying p) ----------

section("Demo 3: Robustness probe (varying p, original vs stable)")

robustness_results <- data.frame()
for (p_val in c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)) {
  set.seed(1234)
  d <- simdat(n_var = p_val)
  true_adj <- build_chain_adjacencies(p_val)

  # Original
  res_o <- tryCatch(
    causnet(d, alpha = 0.05),
    error = function(e) NULL
  )

  # Stable
  res_s <- tryCatch(
    causnet_stable(d, tau = 1, pi_thr = 0.75, B = 100, seed = 1234),
    error = function(e) NULL
  )

  orig_status <- if (is.null(res_o)) "FAILED" else "OK"
  stable_status <- if (is.null(res_s)) "FAILED" else "OK"

  orig_TP <- orig_FP <- stable_TP <- stable_FP <- NA
  if (!is.null(res_o)) {
    m <- evaluate_network(get_component1_edges(res_o), true_adj)
    orig_TP <- m["TP"]; orig_FP <- m["FP"]
  }
  if (!is.null(res_s)) {
    m <- evaluate_network(get_component1_edges(res_s), true_adj)
    stable_TP <- m["TP"]; stable_FP <- m["FP"]
  }

  robustness_results <- rbind(robustness_results, data.frame(
    p = p_val,
    orig_status = orig_status, orig_TP = orig_TP, orig_FP = orig_FP,
    stable_status = stable_status, stable_TP = stable_TP, stable_FP = stable_FP
  ))
}

cat("\n")
print(robustness_results, row.names = FALSE)


# ---------- 5. Demo 4: Repeated experiments (50 runs at p=10) ----------

section("Demo 4: Repeated experiments (50 runs, p=10, n=300)")

n_replicates <- 50
seeds <- 1:n_replicates

results_orig   <- matrix(NA, nrow = n_replicates, ncol = 3,
                         dimnames = list(NULL, c("TP", "FP", "FN")))
results_stable <- matrix(NA, nrow = n_replicates, ncol = 3,
                         dimnames = list(NULL, c("TP", "FP", "FN")))

cat("Running", n_replicates, "replications...\n")
for (i in seq_along(seeds)) {
  set.seed(seeds[i])
  d <- simdat(n_var = 10)
  true_adj <- build_chain_adjacencies(10)

  # Original
  res_o <- tryCatch(
    causnet(d, alpha = 0.05),
    error = function(e) NULL
  )
  if (!is.null(res_o)) {
    results_orig[i, ] <- evaluate_network(get_component1_edges(res_o), true_adj)
  }

  # Stable (suppress per-run messages for cleaner output)
  res_s <- tryCatch(
    suppressMessages(
      causnet_stable(d, tau = 1, pi_thr = 0.75, B = 100, seed = seeds[i])
    ),
    error = function(e) NULL
  )
  if (!is.null(res_s)) {
    results_stable[i, ] <- evaluate_network(get_component1_edges(res_s), true_adj)
  }

  if (i %% 10 == 0) cat(sprintf("  ... %d / %d done\n", i, n_replicates))
}

# Summarize
summarize_metrics <- function(mat, label) {
  valid <- complete.cases(mat)
  n_valid <- sum(valid)
  means <- colMeans(mat[valid, , drop = FALSE])
  sds   <- apply(mat[valid, , drop = FALSE], 2, sd)

  cat(sprintf("\n%s (n_valid = %d / %d):\n", label, n_valid, nrow(mat)))
  cat(sprintf("  TP: mean = %.2f, sd = %.2f\n", means["TP"], sds["TP"]))
  cat(sprintf("  FP: mean = %.2f, sd = %.2f\n", means["FP"], sds["FP"]))
  cat(sprintf("  FN: mean = %.2f, sd = %.2f\n", means["FN"], sds["FN"]))

  invisible(list(mean = means, sd = sds, n_valid = n_valid))
}

s_orig   <- summarize_metrics(results_orig,   "Original causnet (alpha=0.05)")
s_stable <- summarize_metrics(results_stable, "causnet_stable (tau=1, pi_thr=0.75)")

# Statistical comparison: paired test on FP (only on rows where both succeeded)
both_valid <- complete.cases(results_orig) & complete.cases(results_stable)
n_paired <- sum(both_valid)

if (n_paired > 0) {
  fp_diff <- results_orig[both_valid, "FP"] - results_stable[both_valid, "FP"]
  cat(sprintf("\nPaired comparison on FP (n_paired = %d):\n", n_paired))
  cat(sprintf("  Mean FP reduction: %.2f (sd = %.2f)\n", mean(fp_diff), sd(fp_diff)))

  if (n_paired >= 5) {
    # Wilcoxon signed-rank test (non-parametric, robust to ties / small n)
    wt <- tryCatch(
      wilcox.test(results_orig[both_valid, "FP"],
                  results_stable[both_valid, "FP"],
                  paired = TRUE, exact = FALSE),
      error = function(e) NULL
    )
    if (!is.null(wt)) {
      cat(sprintf("  Wilcoxon signed-rank test p-value: %.4f\n", wt$p.value))
    }
  }
}


# ---------- Final summary ----------

section("Final Summary")

cat("\nDemo 1 (single run, p=10):\n")
cat(sprintf("  Original FP: %d, Stable FP: %d\n",
            metrics_orig_10["FP"], metrics_stable_10["FP"]))

cat("\nDemo 3 (robustness across p):\n")
n_orig_failed <- sum(robustness_results$orig_status == "FAILED")
n_stable_failed <- sum(robustness_results$stable_status == "FAILED")
cat(sprintf("  Original failed on %d / %d values of p\n",
            n_orig_failed, nrow(robustness_results)))
cat(sprintf("  Stable failed on %d / %d values of p\n",
            n_stable_failed, nrow(robustness_results)))

cat("\nDemo 4 (50 replications, p=10):\n")
cat(sprintf("  Original mean FP: %.2f (sd %.2f)\n",
            s_orig$mean["FP"], s_orig$sd["FP"]))
cat(sprintf("  Stable   mean FP: %.2f (sd %.2f)\n",
            s_stable$mean["FP"], s_stable$sd["FP"]))

cat("\n[Test script complete]\n")
