
predict.suffpcr <- function(object, newdata = NULL, response = NULL, i = NULL,
                            d = NULL, ...){

  family <- object$family
  assertthat::assert_that(
    is.null(newdata) || ncol(object$Xtrain) == ncol(newdata),
    msg = paste("In predict: if newdata is provided it must have the same",
                "number of columns as the original Xtrain."))
  if (is.null(newdata)) {
    newdata <- object$Xtrain
  } else {
    newdata <- scale(newdata, object$xmeans, object$xsd)
  }
  s <- grab_idx(object$nsol, object$d, i, d)
  mult <- length(s) > 1
  bhat <- object$betahat[-c(1:2), s]
  inter <- object$intercept[-c(1:2), s]
  linterms <- object$ymean + Xtrain %*% bhat
  if (family == "logistic") {
    probs = logistic(linterms)
    mse.train[k, i] = mean((Ytrain.prob - Ytrain)^2)

    cutoffs = sort(Ytrain.prob)
    accuracy = rep(NA, n)
    for (j in 1:n){
      prediction = ifelse(Ytrain.prob >= cutoffs[j], 1, 0)
      accuracy[j] = mean(Ytrain == prediction) * 100
    }
    accuracy.train[k, i] = max(accuracy)
    cutoffs = cutoffs[which.max(accuracy)]
    cutoff.best = cutoffs[which.min(abs(cutoffs - 0.5))]
    # cutoff.best = 0.5
  }
}



grab_idx <- function(nsol, modd, i, d){
  ii <- rep(seq_len(nsol), times = modd)
  dd <- rep(seq_len(modd), each = nsol)
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
