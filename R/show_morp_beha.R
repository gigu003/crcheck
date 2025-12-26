#' Show morphology–behavior rules
#'
#' Show valid combinations of ICD-O morphology and behavior codes according to
#' ICD-O-3 rules.
#'
#' @template morp
#' @template beha
#'
#' @return A character vector of valid morphology–behavior combinations
#'   (e.g., "8000/3").
#' @export
#'
#' @examples
#' show_morp_beha(morp = 8000:8140)
#' show_morp_beha(morp = c(8000, 8010), beha = c(2, 3))
show_morp_beha <- function(morp, beha = c(1:3, 9)) {
  codes <- IARC_BehaMorpRule[[1]]
  
  # Ensure morp and beha are character for proper comparison
  morp <- formatC(morp, width = 4, flag = "0") # pad with leading zeros
  beha <- as.character(beha)
  
  p_morp <- substr(codes, 1, 4)
  p_beha <- substr(codes, 6, 6)
  
  codes[p_morp %in% morp & p_beha %in% beha]
}
