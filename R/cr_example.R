#' Example Cancer Registry Data
#'
#' A cancer registry data frame prepared for demonstrating the validation
#' functions in `crcheck`. The data may contain invalid or internally
#' inconsistent values so that checks can produce illustrative results.
#'
#' @format A data frame with 28,665 rows and 15 variables:
#' \describe{
#'   \item{registr}{Character registry identifier.}
#'   \item{sex}{Character sex code.}
#'   \item{birthda}{Date of birth.}
#'   \item{inciden}{Date of cancer incidence.}
#'   \item{topo}{Character ICD-O-3 topography code.}
#'   \item{morp}{Character ICD-O-3 morphology code.}
#'   \item{beha}{Character ICD-O-3 behaviour code.}
#'   \item{grad}{Character tumour grade code.}
#'   \item{basi}{Character basis-of-diagnosis code.}
#'   \item{icd10}{Character ICD-10 code.}
#'   \item{mult}{Integer multiple-primary indicator.}
#'   \item{status}{Character vital-status code.}
#'   \item{caus}{Character cause-of-death code.}
#'   \item{deathda}{Date of death.}
#'   \item{lastcontact}{Date last known vital status.}
#' }
#'
#' @examples
#' data(cr_example)
#' head(cr_example)
"cr_example"
