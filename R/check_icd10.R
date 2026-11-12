#' Check ICD-10 consistency with ICD-O-3 codes
#'
#' Checks whether an existing ICD-10 code is consistent with the ICD-10 code
#' converted from ICD-O-3 topography, morphology and behaviour codes.
#'
#' @template topo
#' @template morp
#' @template beha
#' @template icd10
#' @template quiet
#'
#' @details
#' Inputs are standardized before comparison. Records are reported only when
#' both the existing ICD-10 code and the converted ICD-10 code are available
#' and differ. Missing, invalid or unmatched values are left to the relevant
#' single-item checks.
#'
#' @template return
#' @export
#'
#' @examples
#' topo <- c("C50.9", "C18.0")
#' morp <- c(8500, 8000)
#' beha <- c(3, 0)
#' icd10 <- c("C34.9", "D12.0")
#' res <- check_icd10(topo, morp, beha, icd10)
#' print(res)
check_icd10 <- function(topo, morp, beha, icd10, quiet = FALSE) {
  check_equal_length(
    topo,
    morp,
    beha,
    icd10,
    arg_names = c("topo", "morp", "beha", "icd10")
  )
  check_flag(quiet, "quiet")

  topo_std <- std_topo(topo)
  morp_std <- std_morp(morp)
  beha_std <- std_beha(beha)
  icd10_std <- std_icd10(icd10)
  expected <- icdo3_to_icd10(topo_std, morp_std, beha_std)

  comparable <- !is.na(icd10_std) & !is.na(expected)
  check <- which(comparable & icd10_std != expected)
  values <- paste(
    topo_std[check],
    morp_std[check],
    beha_std[check],
    icd10_std[check],
    sep = "/"
  )

  res <- structure(
    list(
      check_item = "Topo/Morp/Beha/ICD10",
      check = check,
      data = values,
      type = rep("E-ICD10", length(check)),
      group = rep("G-01", length(check))
    ),
    class = c("check", "icd10/topo/morp/beha")
  )

  if (!quiet) {
    summary(res)
  }
  invisible(res)
}
