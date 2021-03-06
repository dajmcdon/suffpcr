% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{predict.suffPCR}
\alias{predict.suffPCR}
\title{Method for returning predictions of a fitted suffPCR object}
\usage{
\method{predict}{suffPCR}(
  object,
  newdata = NULL,
  type = c("link", "response"),
  i = NULL,
  d = NULL,
  ...
)
}
\arguments{
\item{object}{a suffPCR object, created by \code{\link[=suffPCR]{suffPCR()}}}

\item{newdata}{optionally, a matrix of variables with
which to predict. If omitted, the fitted linear predictors are used.}

\item{type}{the type of prediction required. The default is on the scale of
the linear predictors; the alternative "response" is on the scale of the
response variable. Thus for a default binomial model the default
predictions are of log-odds (probabilities on logit scale) and
type = "response" gives the predicted probabilities.}

\item{i}{optional vector of lambda indices to retrieve}

\item{d}{optional vector of d indices to retrieve}

\item{...}{ignored}
}
\value{
a matrix (or vector) of predicted values. Each column corresponds to
one lambda x d estimate
}
\description{
Method for returning predictions of a fitted suffPCR object
}
