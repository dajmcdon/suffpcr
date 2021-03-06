% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{miller}
\alias{miller}
\title{Gene expression and patient clinical data for breast cancer}
\format{
A list with  named components:
\describe{
\item{x}{a named matrix with 253 rows and 11331 columns containing gene
expression data. Column names are
patient ids to match the clinical data while row names provide
information about the measured genes.}
\item{clinical}{data frame of clinical data for the patients (age,
survival status, lymph node involvement, age, subtype, etc.)}
}
}
\source{
Lance D Miller et al. An expression signature for p53 status in
human breast cancer predicts mutation status, transcriptional effects,
and patient survival. \emph{Proceedings of the National Academy of Sciences},
102(38): 13550–13555, 2005.
}
\usage{
miller
}
\description{
A dataset containing gene expression measurements along with a number of
clinical variables
}
\keyword{datasets}
