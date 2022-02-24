
#' Gene expression and survival data for acute myeloid leukemia
#'
#' A dataset containing gene expression measurements, how long the patient
#' survived, whether they survived, and information about the genes.
#'
#' @format A list with 4 named components:
#' \describe{
#'   \item{x}{gene expression measurements for 116 patients on 6283 genes}
#'   \item{gene_info}{a character vector containing descriptions of the genes}
#'   \item{survivial_time}{time in days the patient survived, possibly right
#'     sensored}
#'   \item{survival_status}{a zero/one indicator of whether the patient died}
#' }
#' @source L Bullinger et al. Gene expression profiling identifies new subclasses
#'  and improves outcome prediction in adult myeloid leukemia.
#'  _The New England Journal of Medicine_, 350(16):1605–1616, 2004.
"aml"


#' Gene expression and survival data for breast cancer
#'
#' A dataset containing gene expression measurements, time to disease recurrence,
#' whether the prognosis was "good" or "poor", and information about the genes.
#'
#' @format A list with 4 named components:
#' \describe{
#'   \item{x}{gene expression measurements for 78 patients on 4751 genes}
#'   \item{gene_info}{a character vector containing descriptions of the genes}
#'   \item{survivial_time}{time in months before disease recurrence, possibly right
#'     sensored}
#'   \item{survival_status}{a zero/one indicator of whether the patient
#'     had a "good" or "poor" prognosis based on whether or not disease
#'     recurred within 5 years.
#'   }
#' }
#' @source Laura J Van’t Veer et al. Gene expression profiling predicts clinical
#'    outcome of breast cancer. _Nature_, 415(6871):530, 2002.
"vantveer"


#' Gene expression and patient clinical data for breast cancer
#'
#' A dataset containing gene expression measurements along with a number of
#' clinical variables
#'
#' @format A list with  named components:
#' \describe{
#'   \item{x}{a named matrix with 253 rows and 11331 columns containing gene
#'     expression data. Column names are
#'     patient ids to match the clinical data while row names provide
#'     information about the measured genes.}
#'   \item{clinical}{data frame of clinical data for the patients (age,
#'     survival status, lymph node involvement, age, subtype, etc.)}
#' }
#' @source Lance D Miller et al. An expression signature for p53 status in
#'   human breast cancer predicts mutation status, transcriptional effects,
#'   and patient survival. _Proceedings of the National Academy of Sciences_,
#'   102(38): 13550–13555, 2005.
"miller"

#' Gene expression and patient clinical data for dlbcl
#'
#' A dataset containing gene expression measurements, how long the patient
#' survived, whether they survived, and information about the genes for patients
#' with diffuse large-B-cell lymphoma. This data can be found in a variety of
#' R packages.
#'
#' @format A list with  named components:
#' \describe{
#'   \item{x}{gene expression measurements for 240 patients on 7399 genes}
#'   \item{gene_info}{a tibble containing descriptions of the genes}
#'   \item{survivial_time}{time in years the patient survived, possibly right
#'     sensored}
#'   \item{survival_status}{a zero/one indicator of whether the patient died}
#' }
#' @source Andreas Rosenwald et al. The use of molecular profiling to predict
#'   survival after chemotherapy for diffuse large-B-cell lymphoma.
#'   _New England Journal of Medicine_, 346(25):1937–1947, 2002.
#' @importFrom tibble tibble
"dlbcl"

#' Micro-RNA expression and patient data for nsclc
#'
#' Gene expression and patient clinical data for breast cancer
#'
#' A dataset containing gene expression measurements along with a number of
#' clinical variables
#'
#' @format A list with  named components:
#' \describe{
#'   \item{x}{a named matrix with 123 patients and 939 columns containing gene
#'     expression data. Column names are
#'     patient ids to match the clinical data while row names provide
#'     information about the measured miRNA.}
#'   \item{clinical}{data frame of clinical data for the patients (birth date,
#'     subtype, relapse information, chemo treatment, surgery, etc.)}
#' }
#' @source Vladimir Lazar et al. Integrated molecular portrait of non-small
#'   cell lung cancers. _BMC medical genomics_, 6(1):53, 2013.
"nsclc"

