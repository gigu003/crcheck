#' Detect possible multiple primary cancers
#'
#' Applies the IARC/IACR 2004 international rules for multiple primary cancers
#' to tumour records grouped by patient identifier. The function compares all
#' tumour pairs within each patient and reports pairs that may represent
#' multiple primaries for incidence and survival reporting.
#'
#' @param x A data frame containing tumour records.
#' @param id A single character string giving the patient identifier column.
#' @param topo A single character string giving the ICD-O-3 topography column.
#' @param morp A single character string giving the ICD-O-3 morphology column.
#' @param inciden Optional single character string giving the diagnosis date
#'   column. Dates are used only for ordering output; the IARC/IACR rules do not
#'   use time interval to recognise multiple primaries.
#' @param laterality Optional single character string giving the laterality
#'   column. This is reserved for future extensions and is not used by the
#'   incidence and survival reporting rules.
#' @param record_recommendations Logical. If `TRUE`, also apply selected
#'   recommendations for recording, such as different fourth-character
#'   subcategories of colon (`C18`) and skin (`C44`). The default `FALSE` applies
#'   the reporting rules only.
#' @param quiet Logical. If `FALSE`, print a short summary.
#'
#' @return A data frame. Each row is a pair of tumour records within the same
#'   patient that may represent multiple primary cancers. The columns include
#'   patient identifier, row indices, topography and morphology values, mapped
#'   rule groups, rule code, and reason.
#'
#' @details
#' The implementation follows the IARC/IACR rules in *Multiple Primary
#' Neoplasms* (July 2004): recognition of multiple primaries does not depend on
#' time; selected topography code groups are considered one site; systemic or
#' multicentric cancers are counted once per individual; different morphology
#' groups are regarded as multiple cancers; and non-specific morphology groups
#' are ignored when a more specific morphology is available for the same site.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   id = c("P1", "P1", "P2", "P2"),
#'   topo = c("C18.0", "C34.9", "C50.9", "C50.9"),
#'   morp = c(8140, 8140, 8140, 8070),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_multiple_primary(df, quiet = TRUE)
check_multiple_primary <- function(
  x,
  id = "id",
  topo = "topo",
  morp = "morp",
  inciden = NULL,
  laterality = NULL,
  record_recommendations = FALSE,
  quiet = FALSE
) {
  check_data_frame(x)
  check_single_string(id, "id")
  check_single_string(topo, "topo")
  check_single_string(morp, "morp")
  check_single_string(inciden, "inciden", allow_null = TRUE)
  check_single_string(laterality, "laterality", allow_null = TRUE)
  check_flag(record_recommendations, "record_recommendations")
  check_flag(quiet, "quiet")

  check_required_columns_ci(x, c(id, topo, morp))
  if (!is.null(inciden)) {
    check_required_columns_ci(x, inciden)
  }
  if (!is.null(laterality)) {
    check_required_columns_ci(x, laterality)
  }

  id_value <- get_column_ci(x, id)
  topo_value <- mp_standardize_topo(get_column_ci(x, topo))
  morp_value <- mp_standardize_morp(get_column_ci(x, morp))
  inciden_value <- if (is.null(inciden)) NULL else get_column_ci(x, inciden)

  topo_group <- mp_topo_group(topo_value)
  morp_group <- mp_morp_group(morp_value)

  if (!is.null(inciden_value)) {
    inciden_date <- parse_registry_date(inciden_value, arg = inciden)
  } else {
    inciden_date <- as.Date(rep(NA_character_, nrow(x)))
  }

  rows_by_id <- split(seq_len(nrow(x)), id_value, drop = TRUE)
  rows_by_id <- rows_by_id[!is.na(names(rows_by_id)) & names(rows_by_id) != ""]

  out <- vector("list", 0L)
  for (patient_id in names(rows_by_id)) {
    rows <- rows_by_id[[patient_id]]
    if (length(rows) < 2L) {
      next
    }

    ord <- order(inciden_date[rows], rows, na.last = TRUE)
    rows <- rows[ord]
    pairs <- utils::combn(rows, 2L)
    pair_res <- vector("list", ncol(pairs))

    for (j in seq_len(ncol(pairs))) {
      pair_res[[j]] <- mp_compare_pair(
        row_1 = pairs[1L, j],
        row_2 = pairs[2L, j],
        patient_id = patient_id,
        topo = topo_value,
        morp = morp_value,
        topo_group = topo_group,
        morp_group = morp_group,
        record_recommendations = record_recommendations
      )
    }
    pair_res <- pair_res[!vapply(pair_res, is.null, logical(1))]
    out <- c(out, pair_res)
  }

  res <- mp_empty_result()
  if (length(out) > 0L) {
    res <- do.call(rbind, out)
    rownames(res) <- NULL
  }

  class(res) <- c("multiple_primary_check", "data.frame")

  if (!quiet) {
    mp_summary(res)
  }

  res
}

check_single_string <- function(x, arg = "x", allow_null = FALSE) {
  if (is.null(x) && allow_null) {
    return(invisible(TRUE))
  }

  if (!is.character(x) || length(x) != 1L || is.na(x) || x == "") {
    stop("`", arg, "` must be a single non-missing string.", call. = FALSE)
  }

  invisible(TRUE)
}

check_required_columns_ci <- function(x, cols) {
  missing_cols <- cols[!tolower(cols) %in% tolower(names(x))]
  if (length(missing_cols) > 0L) {
    stop(
      "The following columns are missing from `x`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

mp_empty_result <- function() {
  data.frame(
    id = character(0),
    row_1 = integer(0),
    row_2 = integer(0),
    topo_1 = character(0),
    topo_2 = character(0),
    morp_1 = integer(0),
    morp_2 = integer(0),
    topo_group_1 = character(0),
    topo_group_2 = character(0),
    morp_group_1 = character(0),
    morp_group_2 = character(0),
    rule = character(0),
    reason = character(0),
    stringsAsFactors = FALSE
  )
}

mp_standardize_topo <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  x <- gsub("[[:space:]]+", "", x)
  x <- add_decimal(x)
  x
}

mp_standardize_morp <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  res <- rep(NA_integer_, length(x))
  idx <- !is.na(x) & grepl("[0-9]{4}", x)
  res[idx] <- suppressWarnings(as.integer(sub(
    "^.*?([0-9]{4}).*$",
    "\\1",
    x[idx]
  )))
  res
}

mp_topo_main <- function(x) {
  x <- mp_standardize_topo(x)
  main <- rep(NA_character_, length(x))
  idx <- grepl("^C[0-9]{2}(\\.[0-9])?$", x)
  main[idx] <- substr(x[idx], 1L, 3L)
  main
}

mp_topo_group <- function(x) {
  main <- mp_topo_main(x)
  res <- main
  topo_rules <- IARC_MultiplePrimaryRule$topo

  for (i in seq_along(topo_rules)) {
    rule_main <- unique(substr(topo_rules[[i]], 1L, 3L))
    res[!is.na(main) & main %in% rule_main] <- sprintf("T-%02d", i)
  }

  res
}

mp_morp_group <- function(x) {
  x <- mp_standardize_morp(x)
  res <- rep(NA_character_, length(x))
  morp_rules <- IARC_MultiplePrimaryRule$morp

  for (i in seq_along(morp_rules)) {
    idx <- !is.na(x) & x %in% morp_rules[[i]]
    res[idx] <- names(morp_rules)[i]
  }

  res
}

mp_compare_pair <- function(
  row_1,
  row_2,
  patient_id,
  topo,
  morp,
  topo_group,
  morp_group,
  record_recommendations = FALSE
) {
  tg1 <- topo_group[row_1]
  tg2 <- topo_group[row_2]
  mg1 <- morp_group[row_1]
  mg2 <- morp_group[row_2]

  if (any(is.na(c(tg1, tg2, mg1, mg2)))) {
    return(NULL)
  }

  nonspecific <- IARC_MultiplePrimaryRule$nonspecific_morp_groups
  systemic <- IARC_MultiplePrimaryRule$systemic_morp_groups

  rule <- NA_character_
  reason <- NA_character_

  if (mg1 %in% systemic && mg2 %in% systemic) {
    return(NULL)
  }

  same_site <- identical(tg1, tg2)
  same_morp_group <- identical(mg1, mg2)
  nonspecific_pair <- xor(mg1 %in% nonspecific, mg2 %in% nonspecific)

  if (!same_site) {
    rule <- "MP-R3"
    reason <- "Different primary sites after applying Table 1 site groups."
  } else if (!same_morp_group && !nonspecific_pair) {
    rule <- "MP-R4.2"
    reason <- "Same primary site but different morphology groups in Table 2."
  } else if (
    record_recommendations &&
      mp_is_colon_or_skin_subsite_pair(topo[row_1], topo[row_2])
  ) {
    rule <- "MP-REC2"
    reason <- "Different fourth-character subcategories of colon or skin."
  } else {
    return(NULL)
  }

  data.frame(
    id = as.character(patient_id),
    row_1 = as.integer(row_1),
    row_2 = as.integer(row_2),
    topo_1 = as.character(topo[row_1]),
    topo_2 = as.character(topo[row_2]),
    morp_1 = as.integer(morp[row_1]),
    morp_2 = as.integer(morp[row_2]),
    topo_group_1 = as.character(tg1),
    topo_group_2 = as.character(tg2),
    morp_group_1 = as.character(mg1),
    morp_group_2 = as.character(mg2),
    rule = rule,
    reason = reason,
    stringsAsFactors = FALSE
  )
}

mp_is_colon_or_skin_subsite_pair <- function(topo_1, topo_2) {
  main <- mp_topo_main(c(topo_1, topo_2))
  topo_std <- mp_standardize_topo(c(topo_1, topo_2))
  if (any(is.na(main)) || main[1L] != main[2L]) {
    return(FALSE)
  }
  if (!main[1L] %in% c("C18", "C44")) {
    return(FALSE)
  }

  subsite <- sub("^C[0-9]{2}\\.", "", topo_std)
  all(grepl("^[0-9]$", subsite)) && subsite[1L] != subsite[2L]
}

mp_summary <- function(x) {
  cat("Possible multiple primary pairs:", nrow(x), "\n")
  if (nrow(x) > 0L) {
    print(as.data.frame(table(x$rule), stringsAsFactors = FALSE))
  }
  cat("\n")
  invisible(x)
}
