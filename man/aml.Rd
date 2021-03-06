% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{aml}
\alias{aml}
\title{Gene expression and survival data for acute myeloid leukemia}
\format{
A list with 4 named components:
\describe{
\item{x}{gene expression measurements for 116 patients on 6283 genes}
\item{gene_info}{a character vector containing descriptions of the genes}
\item{survivial_time}{time in days the patient survived, possibly right
sensored}
\item{survival_status}{a zero/one indicator of whether the patient died}
}
}
\source{
L Bullinger et al. Gene expression profiling identifies new subclasses
and improves outcome prediction in adult myeloid leukemia.
\emph{The New England Journal of Medicine}, 350(16):1605–1616, 2004.
}
\usage{
aml
}
\description{
A dataset containing gene expression measurements, how long the patient
survived, whether they survived, and information about the genes.
}
\keyword{datasets}
