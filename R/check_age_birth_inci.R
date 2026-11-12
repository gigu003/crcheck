#' Check consistency between age and dates.
#'
#' @template age
#' @template birthda
#' @template inciden
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' age <- c(58, 59, 60)
#' birth <- c("1960-08-24", "1956-09-24", "1971-06-30")
#' onset <- c("2024-09-10", "2024-08-10", "2023-09-11")
#' res <- check_age_birth_inci(age, birth, onset)
#' print(res)
check_age_birth_inci <- function(
  age,
  birthda = NULL,
  inciden = NULL,
  quiet = FALSE
) {
  check_equal_length(
    age,
    birthda,
    inciden,
    arg_names = c("age", "birthda", "inciden")
  )

  age_chr <- trimws(as.character(age))
  age_chr[age_chr %in% c("", "NULL", "NA")] <- NA_character_
  age_num <- suppressWarnings(as.numeric(age_chr))
  birthda_date <- parse_registry_date(birthda, arg = "birthda")
  inciden_date <- parse_registry_date(inciden, arg = "inciden")

  result <- lapply(1:2, function(i) {
    if (i == 1) {
      res <- is.na(age_num) | is.na(birthda_date) | is.na(inciden_date)
    } else if (i == 2) {
      age_cal <- calc_registry_age(birthda_date, inciden_date)
      miss_valid <- is.na(age_num) | is.na(birthda_date) | is.na(inciden_date)
      res <- ifelse(!miss_valid, abs(age_num - age_cal) > 1, FALSE)
    }
    return(res)
  })

  loc_age_birth_inci <- lapply(result, which)
  check <- unique(unlist(loc_age_birth_inci))
  groups <- character(length(check))
  values <- character(length(check))
  for (i in 1:2) {
    pos <- loc_age_birth_inci[[i]]
    idx <- match(pos, check)
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) {
      next
    }
    groups[idx] <- paste0(
      ifelse(groups[idx] == "", "G-", paste0(groups[idx], "/")),
      sprintf("%02d", i)
    )
    values[idx] <- paste0(age[pos], "/", birthda[pos], "/", inciden[pos])
  }

  # Construct the result
  res <- structure(
    list(
      check_item = "Age/Birthda/Inciden",
      check = check,
      data = values,
      type = "E-AGED",
      group = groups
    ),
    class = c("check", "age/birthda/inciden")
  )

  if (!quiet) {
    summary(res)
  }
  invisible(res)
}
