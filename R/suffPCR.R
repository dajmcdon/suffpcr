# install packages from github for first time user
# library(devtools)
# install_github("Lei-D/irlba", ref="lei")
# install_github("Lei-D/fps", ref="fps_irlba")


#' Sufficient principal component regression
#'
#' Estimates principal component regression (or classification) under the
#' assumption that the loadings are row-sparse. If this assumption is valid,
#' then the resulting predictors are guaranteed to be irrelevant for predicting
#' a response variable. This results in a sparse model with sparse linear
#' combinations of features used for the final prediction
#'
#' @param Xtrain n by p matrix of features
#' @param Ytrain length n response
#' @param family optional family argument to implement regression ("gaussian",
#'   the default) or classification ("binomial")
#' @param Xtest optional matrix with p columns for test predictions
#' @param Ytest optional test response (if available to compute errors)
#' @param Xvalidate
#' @param Yvalidate
#' @param d target PC dimension
#' @param nsol number of different lambda solutions to examine
#' @param maxnvar optional limit on the number of variables to consider
#' @param screening do we screen as in algorithm 2
#'
#' @return
#' @export
#'
#' @examples
suffPCR <- function(X, Y, family = c("gaussian", "binomial"),
                    d = 3, nsol = 10, maxnvar = ncol(Xtrain),
                    lambda = NULL, lambda_max = NULL,
                    lambda_min = 0.1 * lambda_max,
                    lambda_seq = "loglinear",
                    screening = TRUE){
  n = nrow(X)
  p = ncol(X)
  family <- match.arg(family, c("gaussian", "binomial"))

  # center input data X
  xmeans = colMeans(Xtrain)
  xsd = apply(Xtrain, 2, sd)
  Xtrain = scale(Xtrain, center = xmeans, scale = xsd)

  # output holder
  intercept <- matrix(NA, ncol = nsol*length(d), nrow = 3)
  Betahat = matrix(NA, ncol = nsol*length(d), nrow = p+2)
  Vhat.norm = matrix(NA, ncol = nsol*length(d), nrow = p+2)

  # generate sample covariance matrix
  S = crossprod(Xtrain) / n

  if (is.null(lambda)) {
    lambda = compute_lambda(S, maxnvar, nsol,
                            lambda_max, lambda_min, lambda_seq)
  }

  for(k in seq_len(d)) {
    # run fps
    approximate = fps::fps(S, ndim = d[k], ncomp = d[k],
                           lambda = lambda,
                           maxnvar = maxnvar, maxiter = 100)
    # check on each solution
    for (i in seq_len(nsol)) {
      # compute row-wise l2 norm of vhat
      vhat.norm = diag(approximate$projection[[i]])
      Vhat.norm[1, (k-1) * nsol + i] = k
      Vhat.norm[2, (k-1) * nsol + i] = i
      Vhat.norm[3:(p+2), (k-1) * nsol + i] = vhat.norm
      if (screening == TRUE) {
        l = sort(diag(approximate$projection[[i]]))
        t <- ifelse(i == 1, max(l), findt(l))
        # set rows and columns that has small diagonal values to be 0
        small = which(diag(approximate$projection[[i]]) <= t)
        approximate$projection[[i]][small, ] = 0
        approximate$projection[[i]][ ,small] = 0
      }
      if (all(approximate$projection[[i]] == 0)) {
        betahat = rep(0, p) # use ymean to predict
        inter <- 0
      } else {
        # decompose projection matrix
        decomp = svd(approximate$projection[[i]], nu = d[k], nv = 0)
        vhat = decomp$u
        vhat[which(abs(vhat) < 1e-10)] = 0
        pchat = Xtrain %*% vhat
        # fit linear model
        model = glm(Ytrain ~ pchat, family = family)
        gamhat = as.matrix(coef(model)[-1], ncol = 1)
        inter <- coef(model)[1]
        betahat = vhat %*% gamhat
      }
      # store outputs
      Betahat[1, (k-1) * nsol + i] = k
      Betahat[2, (k-1) * nsol + i] = i
      Betahat[3:(p+2), (k-1) * nsol + i] = betahat
      intercept[1, (k-1)*nsol + i] = k
      intercept[2, (k-1) * nsol + i] = i
      intercept[3, (k-1) * nsol + i] = intercept
    }
  }

  out <- list(betahat = Betahat,
              Xtrain = Xtrain,
              vhat.norm = Vhat.norm,
              family = family,
              intercept = intercept,
              xmeans = xmeans,
              xsd = xsd,
              d = d, nsol = nsol,
              lambda = lambda)
  class(out) <- "suffPCR"
  return(out)
}
