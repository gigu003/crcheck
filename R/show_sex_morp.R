#' Show sex-specific morphology rules
#'
#' Displays morphology codes that are not accepted for a specific sex
#' according to ICD-O-3 rules. These combinations are biologically implausible
#' and should be flagged during data validation.
#'
#' @param quiet Logical. If `FALSE` (default), a header message is printed.
#'
#' @return A data frame with two columns: `sex` and `morp`, showing disallowed
#'   morphology codes for each sex.
#' @export
#'
#' @examples
#' show_sex_morp()
#' show_sex_morp(quiet = TRUE)
show_sex_morp <- function(quiet = FALSE) {
  if (!quiet) {
    cat("Sex-specific unacceptable morphology codes:\n")
  }
  data.frame(
    sex = c(1L, 2L),
    morp = c(
      compress_morp(unlist(IARC_TopoMorpRule[[1]][23:27])),
      compress_morp(unlist(IARC_TopoMorpRule[[1]][28:29]))
    )
  )
}



