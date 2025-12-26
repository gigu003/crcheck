#' Show age–topography–morphology rules
#'
#' Show the age, topography, and morphology restrictions for a given set of
#' rule groups based on the IARC rules. These are used to flag
#' biologically implausible combinations in cancer registry data.
#'
#' @param group An integer vector (1–36) specifying which rule group(s) to view.
#'
#' @return A a data frame with columns: `age`, `topo`, and `morp`, indicating the age
#'   range, topography codes, and morphology codes associated with each rule.
#' @export
#'
#' @examples
#' show_age_topo_morp(group = 1)
#' show_age_topo_morp(group = c(1, 5, 10))
show_age_topo_morp <- function(group = 1:36) {
  if (!all(group %in% 1:36)) {
    stop("`group` must be integer(s) between 1 and 36.")
  }
  
  age <- unname(unlist(
    lapply(
      IARC_AgeTopoMorpRule[[1]][group],
      compress_morp,
      pattern = "%d"
    )
  ))
  
  topo <- unname(unlist(
    lapply(
      IARC_AgeTopoMorpRule[[2]][group],
      compress_topo
    )
  ))
  
  morp <- unname(unlist(
    lapply(
      IARC_AgeTopoMorpRule[[3]][group],
      compress_morp
    )
  ))
  
  data.frame(
    age = age,
    topo = topo,
    morp = morp
  )
}
