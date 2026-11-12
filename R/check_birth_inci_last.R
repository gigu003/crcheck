#' Check consistency between birth date incidence and Date of last known vital status
#'
#' @template birthda
#' @template inciden
#' @template lastcontact
#' @template quiet
#'
#' @template return
#' @export
#'
#'
check_birth_inci_last <- function(
  birthda,
  inciden,
  lastcontact,
  quiet = FALSE
) {
  check_equal_length(
    birthda,
    inciden,
    lastcontact,
    arg_names = c("birthda", "inciden", "lastcontact")
  )

  birthda_date <- parse_registry_date(birthda, arg = "birthda")
  inciden_date <- parse_registry_date(inciden, arg = "inciden")
  lastcontact_date <- parse_registry_date(lastcontact, arg = "lastcontact")

  # ---- 1. valid data ----
  valid <- !is.na(birthda_date) &
    !is.na(inciden_date) &
    !is.na(lastcontact_date)

  # ---- 2. rules check ----
  rule1 <- valid & birthda_date > inciden_date
  rule2 <- valid & birthda_date > lastcontact_date
  rule3 <- valid & inciden_date > lastcontact_date
  result <- list(rule1, rule2, rule3)

  loc_birth_inci_last <- lapply(result, which)
  check <- unique(unlist(loc_birth_inci_last))
  groups <- character(length(check))
  values <- character(length(check))
  items <- character(length(check))
  for (i in 1:3) {
    pos <- loc_birth_inci_last[[i]]
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
      "Birthda/Inciden",
      "Birthda/Lastcontact",
      "Inciden/Lastcontact"
    )[i]
    items[idx] <- ifelse(
      nchar(groups[idx]) > 4,
      "Birthda/Inciden/Lastcontact",
      items[idx]
    )
    mat <- list(birthda[pos], inciden[pos], lastcontact[pos])
    cols <- list(c(1, 2), c(1, 3), c(2, 3))[[i]]
    values[idx] <- paste0(mat[[cols[1]]], "/", mat[[cols[2]]])
    values[idx] <- ifelse(
      nchar(groups[idx]) > 4,
      paste0(birthda[pos], "/", inciden[pos], "/", lastcontact[pos]),
      values[idx]
    )
  }

  # Construct result
  res <- structure(
    list(
      check_item = items,
      check = check,
      data = values,
      type = "E-CODA",
      group = groups
    ),
    class = c("check", "birth/inci/last")
  )

  if (!quiet) {
    summary(res)
  }

  invisible(res)
}
