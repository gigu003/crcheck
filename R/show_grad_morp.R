#' Show grade–morphology rules
#'
#' Show combinations of tumor grade and morphology codes that are restricted
#' according to the IARC coding rules. These rules help identify biologically
#' implausible combinations.
#'
#' @param group An integer vector (1–11) specifying which rule group(s) to
#'    return.
#'
#' @return A data frame with columns `grad` and `morp`, showing the grade and
#'    corresponding morphology codes.
#' @export
#'
#' @examples
#' show_grad_morp(group = 1)
#' show_grad_morp(group = c(1, 3, 5))
show_grad_morp <- function(group = 1:11) {
  if (!all(group %in% 1:11)) {
    stop("`group` must be integer(s) between 1 and 11.")
  }
  data.frame(
    grad = unname(unlist(
      lapply(
        IARC_GradMorpRule[[1]][group],
        compress_morp,
        pattern = "%d"
      ))),
    morp = unname(unlist(
      lapply(
        IARC_GradMorpRule[[2]][group],
        compress_morp
      )))  
  )
}