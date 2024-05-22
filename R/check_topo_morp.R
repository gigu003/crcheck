#' Check the combination of TOPO and MORP.
#'
#' @param topo TOPO code of ICDO3
#' @param morp MOPR code of ICDO3
#'
#' @return Class of check and topo/morp
#' @export
#'
#' @examples
#' topo <- c("C15.1", "C06.9", "C16.2", "C34.9")
#' morp <- c(8140, 8500, 8500, 8600)
#' check_topo_morp(topo, morp)
check_topo_morp <- function(topo = c("C15.2", "C33.9"), morp = c(8240, 8012)) {
  data <- list(topo = topo, morp = morp)
  check_tm <- function(x) {
    if (x <= 64) {
      morp %in% check_site_morp[[1]][[x]] & topo %nin% check_site_morp[[2]][[x]]
    } else {
      morp %in% check_site_morp[[1]][[x]] & topo %in% check_site_morp[[2]][[x]]
    }
  }
  result <- lapply(1:71, check_tm)
  res_matrix <- do.call(rbind, result)
  groups <- apply(res_matrix, 2, function(x) {
    which(x == TRUE, arr.ind = TRUE)[1]
  })
  groups <- ifelse(is.na(groups), 0, groups)
  groups <- groups + 100
  check <- !Reduce(`|`, result)
  type <- ifelse(check, 1, 2)
  res <- list()
  class(res) <- c("check", "topo/morp")
  res$check_item <- "Site/Histology"
  res$check_version <- "IARC Check Rules 2005"
  res$data <- data
  res$check <- check
  res$type <- factor(type,
    levels = c(1:3),
    labels = c("correct", "warning", "error")
  )
  res$message <- groups
  print(res)
  invisible(res)
}
