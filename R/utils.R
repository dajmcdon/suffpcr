logistic <-  function(x, d = 0, a = 1, c = 0, z = 1) {
  c + (z - c) / (1 + exp(a * (d - x)))
}

logit <- function(p) {
  assertthat::assert_that(p > 0, p < 1,
                          msg = "In logit(p): p must be a probability")
  log(p / (1 - p))
}

compute_lambda <- function(S, maxnvar, nsol,
                           lambda_max = NULL,
                           lambda_min = NULL,
                           lambda_seq = "loglinear"){
  maxoffdiag <- sort(compute_maxoffdiag(S), decreasing = T)
  if (is.null(lambda_max)) lambda_max <- maxoffdiag[1]
  if (is.null(lambda_min)) lambda_min <- maxoffdiag[maxnvar]
  # lambda_min = 0
  lambda <- switch(
    type,
    linear = seq(from = lambda_max, to = lambda_min, length.out = nsol),
    loglinear = lambda_min * log10(seq(1, 10, length.out = nsol)) +
      lambda_max * (1 - log10(seq(1, 10, length.out = nsol)))
  )
  lambda
}



# return the maximum off diagnal absolute value in each row
compute_maxoffdiag <- function(S) {
  # S is the input matrix which is symmetric
  D <- diag(S)
  n <- nrow(S)
  S <- S - Matrix::diag(n, D)

  apply(S, 1, function(x) max(abs(x)))
}
