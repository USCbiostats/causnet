#' BGE scoring function
#'
#' Local BGe score of node \code{y} given the parent set \code{x}, for use as
#' \code{score_fun} in \code{\link{causnet}}. Bigger is better.
#'
#' The local score is \eqn{\log p(D_{\{y\} \cup x}) - \log p(D_x)}, the
#' difference between the BGe log marginal likelihoods of the columns
#' \code{c(y, x)} and of the columns \code{x}, both computed with
#' \code{\link[bnlearn]{score}} (\code{type = "bge"}, bnlearn's default
#' priors). With no parents (\code{x = NA}) it is the BGe score of \code{y}
#' alone. Summed over all nodes of a DAG it equals
#' \code{bnlearn::score(dag, mydat, type = "bge")}.
#'
#' @param y Integer, column index of the node.
#' @param x Integer vector, column indices of the parents, or \code{NA} for no
#'   parents.
#' @param mydat A data.frame of numeric variables.
#'
#' @return numeric score
#' @export
score_bge <- function(y, x, mydat) {
  names <- colnames(mydat)

  if (any(is.na(x))) {
    model <- paste0("[", names[y], "]")
    model.net <- bnlearn::model2network(model)
    return(bnlearn::score(model.net, mydat[y], type = "bge"))
  }

  model <- paste0(paste0("[", paste0(names[x], collapse = "]["), "]"),
                  "[",
                  names[y],
                  "|",
                  paste0(names[x], collapse = ":"),
                  "]")
  model.net <- bnlearn::model2network(model)
  joint <- bnlearn::score(model.net, mydat[c(y, x)], type = "bge")

  parents_model <- paste0("[", paste0(names[x], collapse = "]["), "]")
  parents_net <- bnlearn::model2network(parents_model)
  parents <- bnlearn::score(parents_net, mydat[x], type = "bge")

  joint - parents
}
