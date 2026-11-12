#' Check consistency between Topography, Morphology, Behavior, and Grade.
#'
#' @template grad
#' @template topo
#' @template morp
#' @template beha
#' @template quiet
#'
#' @template return
#' @export
#'
check_grad_topo_morp_beha <- function(grad, topo, morp, beha, quiet = FALSE) {
  check_equal_length(
    grad,
    topo,
    morp,
    beha,
    arg_names = c("grad", "topo", "morp", "beha")
  )
  check_flag(quiet, "quiet")

  tbgr_rule <- ENCR_TopoMorpBehaGradRule
  grad <- as.integer(grad)
  topo <- add_decimal(topo)
  morp <- as.integer(gsub("[^0-9]", "", morp))
  beha <- as.integer(beha)
  result <- lapply(1:5, function(x) {
    topo %in%
      tbgr_rule[[1]][[x]] &
      morp %in% tbgr_rule[[2]][[x]] &
      beha %in% tbgr_rule[[3]][[x]] &
      grad %nin% tbgr_rule[[4]][[x]]
  })
  loc_tgbr <- lapply(result, which)
  check <- unique(unlist(loc_tgbr))
  groups <- character(length(check))
  values <- character(length(check))
  for (i in 1:5) {
    pos <- loc_tgbr[[i]]
    idx <- match(pos, check)
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) {
      next
    }
    groups[idx] <- paste0(
      ifelse(groups[idx] == "", "G-", paste0(groups[idx], "/")),
      sprintf("%02d", i)
    )
    values[idx] <- paste0(
      grad[pos],
      "/",
      topo[pos],
      "/",
      morp[pos],
      "/",
      beha[pos]
    )
  }

  # Construct result
  res <- structure(
    list(
      check_item = "Grad/Topo/Morp/Beha",
      check = check,
      data = values,
      type = "W-TGBR",
      group = groups
    ),
    class = c("check", "Grad/Topo/Morp/Beha")
  )

  if (!quiet) {
    summary(res)
  }
  invisible(res)
}
