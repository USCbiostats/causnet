#' BGE scoring function
#'
#' @param y y
#' @param x x
#' @param mydat data
#'
#' @return numeric score
#' @export
score_bge <- function(y, x, mydat) {
  names <- colnames(mydat)

  if (any(is.na(x))) {
    model <- paste0("[",
                    names[y],
                    "]")
    model.net <- bnlearn::model2network(model)
    model_data <- mydat[y]
  } else {
    model <- paste0(paste0("[", paste0(names[x], collapse = "]["), "]"),
                    "[",
                    names[y],
                    "|",
                    paste0(names[x], collapse = ":"),
                    "]")
    model.net <- bnlearn::model2network(model)
    model_data <- mydat[c(y, x)]
  }

  bnlearn::modelstring(model.net) <- model

  res <- bnlearn::score(model.net, model_data, type = "bge")
  -res
}
