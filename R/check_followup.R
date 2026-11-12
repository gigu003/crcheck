#' Check consistency between follow up variables
#' @template basi
#' @param status description
#' @template inciden
#' @param deathda Deathda.
#' @template lastcontact
#' @template quiet
#'
#' @template return
#' @export
#'
#'
check_followup <- function(
  basi,
  status,
  inciden,
  deathda,
  lastcontact,
  quiet = FALSE
) {
  check_equal_length(
    basi,
    status,
    inciden,
    deathda,
    lastcontact,
    arg_names = c("basi", "status", "inciden", "deathda", "lastcontact")
  )

  inciden_date <- parse_registry_date(inciden, arg = "inciden")
  deathda_date <- parse_registry_date(deathda, arg = "deathda")
  lastcontact_date <- parse_registry_date(lastcontact, arg = "lastcontact")

  # ---- 1. rules check ----
  nmiss_basi <- !is.na(basi) & !is.na(inciden_date) & !is.na(deathda_date)
  rule1 <- nmiss_basi & basi == 0 & inciden_date != deathda_date
  rule2 <- !is.na(status) & status == 2 & is.na(deathda_date)
  rule3 <- !is.na(status) & status != 2 & !is.na(deathda_date)
  rule4 <- !is.na(deathda_date) &
    !is.na(lastcontact_date) &
    status == 2 &
    deathda_date != lastcontact_date
  rule5 <- !is.na(inciden_date) &
    !is.na(deathda_date) &
    inciden_date > deathda_date
  result <- list(rule1, rule2, rule3, rule4, rule5)

  loc_follow <- lapply(result, which)
  check <- unique(unlist(loc_follow))
  groups <- character(length(check))
  values <- character(length(check))
  items <- character(length(check))
  for (i in 1:5) {
    pos <- loc_follow[[i]]
    idx <- match(pos, check)
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) {
      next
    }
    groups[idx] <- paste0(
      ifelse(groups[idx] == "", "G-", paste0(groups[idx], "/")),
      sprintf("%02d", i)
    )
    items[idx] <- c(
      "Basi/Inciden/Deathda",
      "Status/Deathda",
      "Status/Deathda",
      "Status/Deathda/Lastcontact",
      "Inciden/Deathda"
    )[i]
    mat <- list(
      basi[pos],
      inciden[pos],
      status[pos],
      deathda[pos],
      lastcontact[pos]
    )
    cols <- list(c(1, 2, 4), c(3, 4), c(3, 4), c(3, 4, 5), c(2, 4))[[i]]
    values[idx] <- do.call(paste, c(mat[cols], sep = "/"))
  }

  # Construct result
  res <- structure(
    list(
      check_item = items,
      check = check,
      data = values,
      type = "E-FOUP",
      group = groups
    ),
    class = c("check", "followup")
  )

  if (!quiet) {
    summary(res)
  }
  invisible(res)
}
