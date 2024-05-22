#' Check consistency between sex and site code
#'
#' @param sex sex code
#' @param topo cancer site code
#'
#' @return class of 'check'
#' @export
#'
#' @examples
#' sex <- c(1, 1, 1, 2, 2, 2)
#' topo <- c("C51.0", "C53.0", "C57.4", "C60.0", "C60.9", "C63.9")
check_sex_topo <- function(sex, topo) {
  male <- sex == 1 & substr(topo, 1, 3) %in% paste0("C", c(51:58))
  female <- sex == 2 & substr(topo, 1, 3) %in% paste0("C", c(60:63))
  check <- !male & !female
  type <- ifelse(male | female, 3, 1)
  message <- ifelse(male, 181, ifelse(female, 182, 0))
  res <- list()
  class(res) <- c("check", "sex/topo")
  res$check_item <- "Sex/Site"
  res$check_version <- "IARC Tech Report 2005"
  res$check <- check
  res$type <- type
  res$message <- message
  print(res)
  invisible(res)
}
