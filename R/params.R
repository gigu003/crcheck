#' Common Return Value for check Functions
#'
#' @return A list object of class `check` containing:
#' \itemize{
#'   \item \code{check_item}: Character contains item or items combination
#'          being checked.
#'   \item \code{check_standard}: Character contains the standard used
#'   \item \code{check_version}:  Character contains the version of the
#'          standard used.
#'   \item \code{data}: A list contains the input data.
#'   \item \code{check}: A logical vector indicating whether each code passed
#'          the checks.
#'   \item \code{type}: A numeric vector categorizing validation results, 
#'       \itemize{
#'         \item \code{1}: Correct
#'         \item \code{2}: Warning
#'        \item \code{3}: Error
#'        }
#'   \item \code{error_code}: A character vector providing error or warning codes.
#'   \item \code{error_desc}: A numeric vector contains description codes for
#'          specific information.
#' }
#' @name comm_return_value
#' @keywords internal
NULL

#' Standard
#' 
#' @param std Character, indicate the standard used to check the validation of
#'        the item or items combination, options are "iacr" or "encr".
#' @name param_std
#' @keywords internal
NULL

#' show_summary
#' 
#' @param show_summary Logical, whether to display a summary of the validation 
#'                     results. Defaults to `TRUE`. If `FALSE`, only the result 
#'                     object will be returned.
#' @name param_show_summary
#' @keywords internal
NULL

#' sex
#' 
#' @param sex A character vector to be checked whether comply with the gender
#'        coding rules.
#' @name param_sex
#' @keywords internal
NULL

#' Topography
#' 
#' @param topo A character vector to be checked whether comply with the ICD-O-3
#'        topography coding rules, which indicate the topography cancer sites,
#'        e.g.(C50.9). Invalid codes (e.g., missing decimal points or
#'        non-conforming formats) will be flagged during validation. For more
#'        details about the rules, refer to the official ICD-O-3 coding
#'        guidelines: \url{http://www.iacr.com.fr/index.php?Itemid=577}.
#' @name param_topo
#' @keywords internal
NULL

#' Morphology
#' 
#' @param morp A character vector to be checked whether comply with the ICD-O-3
#'        morphology coding rules, which is a 4-digit numerical code ranging
#'        from 8000 to 9993 indicate the cancer histology, but it is not
#'        continuous, some 4-digit codes within this range are not valid codes,
#'        e.g.(8977, 8978), and you can use `gen_morp(8000:9993)` to list all
#'        valid morphology codes. Invalid codes will be flagged during
#'        validation. For more details about the rules, refer to the official
#'        ICD-O-3 coding guidelines: \url{http://www.iacr.com.fr/index.php?Itemid=577}.
#' @name param_morp
#' @keywords internal
NULL

#' Behaviour
#' 
#' @param beha A character vector to be checked whether comply with the ICD-O-3
#'        behaviour coding rules, which is a one-digit numerical code (0, 1, 2,
#'        3, 6, 9), and indicate the cancer behaviour. Invalid codes will be
#'        flagged during validation. For more details about the rules, refer
#'        to the official ICD-O-3 coding guidelines:
#'        \url{http://www.iacr.com.fr/index.php?Itemid=577}.
#' @name param_beha
#' @keywords internal
NULL

#' Grade
#' 
#' @param grad A character vector to be checked whether comply with the ICD-O-3
#'        grade coding rules, which is a one-digit numerical code ranging from
#'        1 to 9, and indicate the grade of cancer development. Invalid codes
#'        will be flagged during validation. For more details about the rules,
#'        refer to the official ICD-O-3 coding guidelines:
#'        \url{http://www.iacr.com.fr/index.php?Itemid=577}.
#' @name param_grad
#' @keywords internal
NULL

#' Basis
#' 
#' @param basi A character vector to be checked whether comply with the basis
#'        coding rules of cancer diagnosis, which is a one-digit numerical code
#'        of (0, 1, 2, 4, 5, 7, 9). Invalid codes
#'        will be flagged during validation.
#' @name param_basi
#' @keywords internal
NULL

#' ICD10
#' 
#' @param icd10 A character vector to be checked whether comply with the ICD10
#'        coding rules in tumor part. Invalid codes will be flagged during
#'        validation. For more details about the rules, refer to the official
#'        ICD10 coding guidelines: \url{https://icd.who.int/browse10/2019/en}.
#' @name param_icd10
#' @keywords internal
NULL

#' Incidence
#' 
#' @param inciden Date vector corresponding to the cancer incidence date.
#' @name param_inciden
#' @keywords internal
NULL

#' Birth date
#' 
#' @param birthda Date vector corresponding to the birth date of cancer
#'        patients.
#' @name param_birthda
#' @keywords internal
NULL

#' Nationalities
#' 
#' @param trib Character vector to be checked whether comply with code of
#'        nationalities of GB3304-91 in China.
#' @name param_trib
#' @keywords internal
NULL

#' Occupation
#' 
#' @param occu Character vector to be checked whether comply with code of
#'        occupation of GB/T2261.4 in China.
#' @name param_occu
#' @keywords internal
NULL