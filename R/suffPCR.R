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
#' @param d target PC dimension
#' @param n_lambda number of different lambda solutions to examine
#' @param maxnvar optional limit on the number of variables to consider
#' @param lambda optional vector of lambda values to use in the penalty
#' @param lambda_max optional largest value of lambda
#' @param lambda_min optional smallest value of lambda, must be non-negative
#'   and less than lambda_max
#' @param lambda_seq should lambda be constructed on a loglinear or linear
#'   scale between the minimum and maximum
#' @param screening do we screen as in algorithm 2
#'
#' @return
#' @export
#'
#' @examples
#' n <- 100
#' p <- 50
#' U <- rnorm(n)
#' V <- c(rnorm(5), rep(0, p - 5))
#' V <- V / sqrt(sum(V^2))
#' bstar <- V * (5 / (5.1))
#' X <- 5 * tcrossprod(U, V) + 0.1 * matrix(rnorm(n * p), n)
#' y <- U + rnorm(n)
#' out <- suffPCR(X, y, d=1:3)
suffPCR <- function(X, Y, family = c("gaussian", "binomial"),
                    d = 3, n_lambda = 10, maxnvar = ncol(X),
                    lambda = NULL, lambda_max = NULL,
                    lambda_min = NULL,
                    lambda_seq = c("loglinear","linear"),
                    screening = TRUE){

  lambda_seq <- match.arg(lambda_seq, c("loglinear","linear"))
  assertthat::assert_that(is.null(lambda) || all(lambda >= 0),
                          msg = "lambda vector must be non-negative.")
  if (!is.null(lambda_max) && !is.null(lambda_min)) {
    assertthat::assert_that(lambda_min >= 0 && lambda_min < lambda_max,
                            msg = paste("lambda_min must be non-negative and",
                                        "strictly less than lambda_max"))
  }
  n = nrow(X)
  p = ncol(X)
  family <- match.arg(family, c("gaussian", "binomial"))
  n_d <- length(d)

  # center input data X
  xmeans = colMeans(X)
  xsd = apply(X, 2, sd)
  X = scale(X, center = xmeans, scale = xsd)

  # output holder
  intercept <- matrix(0, nrow = n_lambda, ncol = n_d)
  Betahat <- matrix(0, nrow = p, ncol = n_lambda * n_d)
  Vhat.norm = matrix(0, nrow = p, ncol = n_lambda * n_d)

  # generate sample covariance matrix
  S = crossprod(X) / n

  if (is.null(lambda)) {
    lambda = compute_lambda(S, maxnvar, n_lambda,
                            lambda_max, lambda_min, lambda_seq)
  } else {
    lambda <- sort(lambda)
  }

  for(k in seq_len(n_d)) {
    # run fps
    approximate = fps::fps(S, ndim = d[k], ncomp = d[k],
                           lambda = lambda,
                           maxnvar = maxnvar, maxiter = 100)

    for (i in seq_len(n_lambda)) {
      # compute row-wise l2 norm of vhat
      vhat.norm = diag(approximate$projection[[i]])
      Vhat.norm[,(k - 1) * n_lambda - i] <- vhat.norm
      if (screening == TRUE) {
        l = sort(vhat.norm)
        t <- ifelse(i == 1, max(l), findt(l))
        # set rows and columns that has small diagonal values to be 0
        small <- (vhat.norm < t)
        approximate$projection[[i]][small, ] <- 0
        approximate$projection[[i]][ ,small] <- 0
      }
      if (all(approximate$projection[[i]] == 0)) {
        betahat = rep(0, p) # use ymean to predict
        inter <- 0
      } else {
        # decompose projection matrix
        decomp = svd(approximate$projection[[i]], nu = d[k], nv = 0)
        vhat = decomp$u
        vhat[abs(vhat) < 1e-10] <-  0
        pchat = X %*% vhat
        # fit linear model
        model = glm(Y ~ pchat, family = family)
        gamhat = as.matrix(coef(model)[-1], ncol = 1)
        inter <- coef(model)[1]
        betahat = vhat %*% gamhat
      }
      # store outputs
      Betahat[,(k - 1) * n_lambda - i] <- betahat
      intercept[i,k] <- inter
    }
  }

  Betahat <- Matrix::drop0(Betahat)
  Vhat.norm <- Matrix::drop0(Vhat.norm)
  nn <- expand.grid(seq_len(n_lambda), d)
  nn <- stringr::str_glue_data(nn, "lamidx{Var1}_d{Var2}")
  colnames(Betahat) <- nn
  colnames(Vhat.norm) <- nn

  out <- list(betahat = Betahat,
              X = X,
              vhat_norm = Vhat.norm,
              family = family,
              intercept = intercept,
              xmeans = xmeans,
              xsd = xsd,
              d = d, n_lambda = n_lambda,
              lambda = lambda)
  class(out) <- "suffPCR"
  return(out)
}
