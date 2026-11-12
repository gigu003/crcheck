#' Check the combination of topography and morphology
#'
#' Validates combinations of topography (ICD-O-3 site) and morphology (ICD-O-3
#' histology) codes using standard tumor coding rules. Supports IARC (2005)
#' standards for assessing whether a given topography/morphology
#' pairing is considered valid, invalid, or discouraged.
#'
#' @template topo
#' @template morp
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' topo <- c("C15.1", "C06.9", "C16.2", "C34.9")
#' morp <- c(8140, 8500, 8500, 8600)
#' res <- check_topo_morp(topo, morp)
#' print(res)
check_topo_morp <- function(topo, morp, quiet = FALSE) {
  check_equal_length(topo, morp, arg_names = c("topo", "morp"))
  check_flag(quiet, "quiet")

  topo <- add_decimal(topo)
  morp <- as.integer(gsub("[^0-9]", "", morp))
  result <- lapply(2:71, check_tm, topo = topo, morp = morp)
  loc_topo_morp <- lapply(result, which)
  check <- unique(unlist(loc_topo_morp))
  groups <- character(length(check))
  values <- paste0(topo[check], "/", morp[check])
  for (i in 1:70) {
    idx <- match(loc_topo_morp[[i]], check)
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) {
      next
    }
    groups[idx] <- paste0(
      ifelse(groups[idx] == "", "G-", paste0(groups[idx], "/")),
      sprintf("%02d", i + 1)
    )
  }

  # Construct result
  res <- structure(
    list(
      check_item = "Topo/Morp",
      check = check,
      data = values,
      type = "W-TOMO",
      group = groups
    ),
    class = c("check", "topo/morp")
  )

  if (!quiet) {
    summary(res)
  }
  invisible(res)
}
