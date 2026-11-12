#' Check consistency between data items of age, site, and morp
#'
#' @template age
#' @template topo
#' @template morp
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' age <- c(30)
#' topo <- c("C61.9")
#' morp <- c(8140)
#' res <- check_age_topo_morp(age, topo, morp)
#' print(res)
check_age_topo_morp <- function(age, topo, morp, quiet = FALSE) {
  check_equal_length(age, topo, morp, arg_names = c("age", "topo", "morp"))
  check_flag(quiet, "quiet")

  age <- as.integer(age)
  topo <- add_decimal(topo)
  morp <- as.integer(gsub("[^0-9]", "", morp))
  un_age_morp <- function(x) {
    agerule <- IARC_AgeTopoMorpRule
    un_age <- agerule[["un_age"]][[x]]
    topo_list <- agerule[["topo"]][[x]]
    morp_list <- agerule[["morp"]][[x]]
    if (length(topo_list) == 0) {
      topo_list <- gen_topo(0:809)
    }
    age %in% un_age & morp %in% morp_list & topo %in% topo_list
  }

  result <- lapply(1:33, un_age_morp)
  loc_age_topo_morp <- lapply(result, which)
  check <- unique(unlist(loc_age_topo_morp))
  groups <- character(length(check))
  values <- character(length(check))
  for (i in 1:33) {
    pos <- loc_age_topo_morp[[i]]
    idx <- match(pos, check)
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) {
      next
    }
    groups[idx] <- paste0(
      ifelse(groups[idx] == "", "G-", paste0(groups[idx], "/")),
      sprintf("%02d", i)
    )
    values[idx] <- paste0(age[pos], "/", topo[pos], "/", morp[pos])
  }

  # Construct result data
  res <- structure(
    list(
      check_item = "Age/Topo/Morp",
      check = check,
      data = values,
      type = "W-AGMT",
      group = groups
    ),
    class = c("check", "age/site/morp")
  )

  if (!quiet) {
    summary(res)
  }

  invisible(res)
}
