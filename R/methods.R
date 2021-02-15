#' Method for returning predictions of a fitted suffpcr object
#'
#' @param object a suffpcr object, created by [suffpcr()]
#' @param newdata optionally, a matrix of variables with
#'   which to predict. If omitted, the fitted linear predictors are used.
#' @param type the type of prediction required. The default is on the scale of
#'   the linear predictors; the alternative "response" is on the scale of the
#'   response variable. Thus for a default binomial model the default
#'   predictions are of log-odds (probabilities on logit scale) and
#'   type = "response" gives the predicted probabilities.
#' @param i optional vector of lambda indices to retrieve
#' @param d optional vector of d indices to retrieve
#' @param ... ignored
#'
#' @return a matrix (or vector) of predicted values. Each column corresponds to
#'   one lambda x d estimate
#' @export
predict.suffpcr <- function(object, newdata = NULL,
                            type = c("link", "response"),
                            i = NULL, d = NULL, ...){
  type <- match.arg(type, c("link","response"))
  family <- object$family
  assertthat::assert_that(
    is.null(newdata) || ncol(object$X) == ncol(newdata),
    msg = paste("In predict: if newdata is provided it must have the same",
                "number of columns as the original Xtrain."))
  if (is.null(newdata)) {
    newdata <- object$X
  } else {
    newdata <- scale(newdata, object$xmeans, object$xsd)
  }
  bhat <- coef(object, i=i, d=d)
  inter <- c(grab_intr(object, i, d))
  linterms <-  newdata %*% bhat + tcrossprod(rep(1, nrow(newdata)), inter)
  if (family == "binomial" && type == "response") linterms <- logistic(linterms)

  return(drop(linterms))
}



#' Method for returning coefficients of a fitted suffpcr object
#'
#' @param object a suffpcr object, created by [suffpcr()]
#' @param i optional vector of lambda indices to retrieve
#' @param d optional vector of d indices to retrieve
#' @param ... ignored
#'
#' @return a (sparse) matrix of coefficients. Each column corresponds to one
#'   lambda x d estimate
#' @export
coef.suffpcr <- function(object, i = NULL, d = NULL, ...){
  s <- grab_idx(object$n_lambda, length(object$d), i, d)
  bhat <- object$betahat[, s]
  bhat
}

grab_intr <- function(object, i, d) {
  nd <- length(object$d)
  if (is.null(i)) i <- seq_len(object$n_lambda)
  if (is.null(d)) d <- seq_len(nd)
  object$intercept[i,d]
}


grab_idx <- function(nlam, nd, i, d){
  ii <- rep(seq_len(nlam), times = nd)
  dd <- rep(seq_len(nd), each = nlam)
  if (is.null(i)) {
    i <- seq_along(ii)
  } else {
    i <- (ii %in% i)
  }
  if (is.null(d)) {
    d <- seq_along(dd)
  } else {
    d <- (dd %in% d)
  }
  d & i
}
