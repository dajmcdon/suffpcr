% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{vantveer}
\alias{vantveer}
\title{Gene expression and survival data for breast cancer}
\format{
A list with 4 named components:
\describe{
\item{x}{gene expression measurements for 78 patients on 4751 genes}
\item{gene_info}{a character vector containing descriptions of the genes}
\item{survivial_time}{time in days the patient survived, possibly right
sensored}
\item{survival_status}{a zero/one indicator of whether the patient died}
}
}
\source{
Laura J Van’t Veer et al. Gene expression profiling predicts clinical
outcome of breast cancer. \emph{Nature}, 415(6871):530, 2002.
}
\usage{
vantveer
}
\description{
A dataset containing gene expression measurements, how long the patient
survived, whether they survived, and information about the genes.
}
\keyword{datasets}
