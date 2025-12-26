#' Check consistency between Topography, Morphology, Behavior, and Grade.
#'
#' @template topo
#' @template morp
#' @template beha
#' @template grad
#' @template quiet
#'
#' @template return
#' @export
#'
check_topo_morp_beha_grad <- function(topo,
                                      morp,
                                      beha,
                                      grad,
                                      quiet = FALSE) {
  tbgr_rule <- ENCR_TopoMorpBehaGradRule
  tbgr <- lapply(1:5, function(x) {
    topo %in% tbgr_rule[[1]][[x]] &
      morp %in% tbgr_rule[[2]][[x]] &
      beha %in% tbgr_rule[[3]][[x]] &
      grad %nin% tbgr_rule[[4]][[x]]
  })
  tbgr <- Reduce('|', tbgr)
  # generate result data
  data <- list(topo = topo, morp = morp, beha = beha, grad = grad)
  check <- !tbgr
  type <- ifelse(check, 1, 2)
  error_code <- ifelse(!check, "W-TBGR", "C")
  error_desc <- ifelse(!check, 301, 0)
  # output result
  res <- list()
  class(res) <- c("check", "topo/morp/beha/grad")
  res$check_item <- "Topo/Morp/Beha/Grad"
  res$check_standard <- "ENCR"
  res$check_version <- "IARC Check Rules 2005"
  res$data <- data
  res$check <- check
  res$type <- type
  res$error_code <- error_code
  res$error_desc <- error_desc
  if (!quiet) {
    summary(res)  
  }
  invisible(res)
}
