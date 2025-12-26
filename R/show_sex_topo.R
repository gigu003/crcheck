#' Show sex-specific topography rules
#'
#' Displays topography codes that are not accepted for a specific sex
#' according to ICD-O-3 rules. These combinations are biologically implausible
#' and should be flagged during data validation.
#'
#' @param quiet Logical. If `FALSE` (default), a header message is printed.
#'
#' @return A data frame with two columns: `sex` and `topo`, showing disallowed
#'   topography codes for each sex.
#' @export
#'
#' @examples
#' show_sex_topo()
#' show_sex_topo(quiet = TRUE)
show_sex_topo <- function(quiet = FALSE) {
  if (!quiet) {
    cat("Sex-specific unacceptable topography codes:\n")
  }
  data.frame(
    sex = c(1L, 2L),
    topo = c(
      compress_topo(gen_topo(510:589)),
      compress_topo(gen_topo(600:639)))
  )
}