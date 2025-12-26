#' Show IARC topography and morphology rule combinations
#'
#' Displays the allowed or disallowed combinations of ICD-O-3 topography and
#' morphology codes based on predefined rule groups from IARC. This is useful
#' for understanding internal cancer registry validation logic and interpreting
#' results from consistency checks.
#'
#' @param family Integer. The rule group (family) index in `IARC_TopoMorpRule`.
#'
#' @return A data frame containing:
#' \describe{
#'   \item{group}{An integer indicating the rule type group: 1, 2, or 3.}
#'   \item{family}{The rule family number.}
#'   \item{morp}{A character string of comma-separated morphology codes.}
#'   \item{topo}{A character string of comma-separated topography codes.}
#' }
#' @export
#'
#' @examples
#' show_topo_morp(3)
#' show_topo_morp(1:71)
show_topo_morp <- function(family) {
  family <- as.integer(family)
  if (any(is.na(family)) ||
      any(family < 1) ||
      any(family > length(IARC_TopoMorpRule[[1]]))) {
    stop("Invalid 'family' index. Please provide a valid rule group number.")
  }
  group <- ifelse(family == 1, 1, ifelse(family <= 64, 2, 3))
  group <- as.integer(group)
  morp <- unname(unlist(lapply(IARC_TopoMorpRule[[1]][family], compress_morp)))
  topo <- unname(unlist(lapply(IARC_TopoMorpRule[[2]][family], compress_topo)))
  data.frame(
    group = group,
    family = family,
    morp = morp,
    topo = topo
  )
}
