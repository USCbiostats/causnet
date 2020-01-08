#' Bigger is better score for node y and parents x
#' @keywords internal
#' @importFrom stats BIC lm
score_bic_lm <- function(y, x, mydat) {
  y_nm <- colnames(mydat)[y]
  if (is.element(x[1], seq_len(ncol(mydat)))) {
    x_nms <- colnames(mydat)[x]
  } else {
    x_nms <- "1"
  }
  fit <- lm(paste0(y_nm, " ~ ", paste(x_nms, collapse = " + ")), data = mydat)
  bic <- - (1 / 2) * BIC(fit)
  return(bic)
}
