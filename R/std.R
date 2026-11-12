#' Standardize ICD-10 codes
#'
#' Extract and standardize ICD-10 codes from raw input strings.
#' The function takes the first 5 characters, removes the decimal
#' suffix (e.g. ".9"), and converts the numeric portion to a
#' standardized ICD-10 code using [gen_icd10()].
#'
#' @param icd10 A character vector of raw ICD-10 codes.
#'
#' @return A character vector of standardized ICD-10 codes.
#'   Invalid or unparseable inputs return `NA`.
#'
#' @details
#' The standardization process follows these steps:
#'
#' 1. Extract the first 5 characters of the input.
#' 2. Remove the decimal suffix pattern (e.g., `.x` or `.X`).
#' 3. Convert the numeric portion: 2-digit numbers prefer xx9
#'    (multiply by 10 and add 9), 3-digit numbers are kept as is.
#' 4. Generate a standardized ICD-10 code using [gen_icd10()] with the
#'    appropriate prefix (`"C"` or `"D"`).
#' 5. If the result is `NA`, retry with an alternative:
#'    2-digit codes try xx0 (subtract 9), 3-digit codes try number + 9.
#'
#' @examples
#' std_icd10(c("C50.9", "C34.1", "C509", "D05.1", NA))
#'
#' @export
std_icd10 <- function(icd10) {
  if (!is.character(icd10)) {
    stop("`icd10` must be a character vector.", call. = FALSE)
  }

  res <- rep(NA_character_, length(icd10))

  # Step 1: Take first 5 characters
  x <- substr(icd10, 1L, 5L)

  # Step 2: Remove ".xX" pattern (dot + single letter at end, e.g. ".x", ".X")
  x <- gsub("\\.[a-zA-Z]$", "", x)

  # Extract init letter and numeric part
  init_letter <- toupper(substr(x, 1L, 1L))
  num_str <- gsub("[^0-9]", "", x)

  # Valid: non-NA input, C/D init, has numeric part
  idx <- !is.na(icd10) & init_letter %in% c("C", "D") & nzchar(num_str)

  if (!any(idx)) {
    return(res)
  }

  num <- as.integer(num_str[idx])
  nch <- nchar(num_str[idx])
  ltr <- init_letter[idx]

  # Step 3: 2-digit * 10 + 9 (prefer xx9), 3-digit keep as is
  num[nch == 2L] <- num[nch == 2L] * 10L + 9L

  # Step 5: Generate ICD-10 separately for C and D
  res_idx <- which(idx)

  for (ini in c("C", "D")) {
    idx_ltr <- ltr == ini
    if (!any(idx_ltr)) {
      next
    }

    nums_ltr <- num[idx_ltr]
    new_codes <- gen_icd10(nums_ltr, init = ini, na_drop = FALSE)

    # Step 6: If result is NA for 2-digit codes, retry with xx0 (-9)
    #         For 3-digit codes, retry with number + 9
    na_codes <- is.na(new_codes)
    if (any(na_codes)) {
      nch_ltr <- nch[idx_ltr]
      nch_na <- nch_ltr[na_codes]
      nums_na <- nums_ltr[na_codes]

      fallback_nums <- nums_na
      fallback_nums[nch_na == 2L] <- fallback_nums[nch_na == 2L] - 9L
      fallback_nums[nch_na == 3L] <- fallback_nums[nch_na == 3L] + 9L

      new_codes[na_codes] <- gen_icd10(
        fallback_nums,
        init = ini,
        na_drop = FALSE
      )
    }

    # Map back to original positions
    res[res_idx[idx_ltr]] <- new_codes
  }

  return(res)
}


std_topo <- function(x) {
  x <- as.character(x)
  x <- toupper(trimws(x))
  x[x %in% c("", "NA", "NULL")] <- NA_character_

  res <- rep(NA_character_, length(x))
  idx <- !is.na(x)
  x_clean <- gsub("\\.", "", x[idx])

  idx_cxxx <- grepl("^C[0-9]{3}$", x_clean)
  if (any(idx_cxxx)) {
    z <- x_clean[idx_cxxx]
    res[which(idx)[idx_cxxx]] <- paste0(
      substr(z, 1L, 3L),
      ".",
      substr(z, 4L, 4L)
    )
  }

  idx_xxx <- grepl("^[0-9]{3}$", x_clean)
  if (any(idx_xxx)) {
    z <- x_clean[idx_xxx]
    res[which(idx)[idx_xxx]] <- paste0(
      "C",
      substr(z, 1L, 2L),
      ".",
      substr(z, 3L, 3L)
    )
  }

  idx_cxx <- grepl("^C[0-9]{2}$", x_clean)
  if (any(idx_cxx)) {
    res[which(idx)[idx_cxx]] <- x_clean[idx_cxx]
  }

  res
}

std_morp <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NULL")] <- NA_character_

  res <- rep(NA_character_, length(x))
  idx <- !is.na(x)
  digits <- gsub("[^0-9]", "", x[idx])
  ok <- nchar(digits) >= 4L
  res[which(idx)[ok]] <- substr(digits[ok], 1L, 4L)
  res
}

std_beha <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NULL")] <- NA_character_

  res <- rep(NA_character_, length(x))
  idx <- !is.na(x)
  ok <- grepl("^[0-9]$", x[idx])
  res[which(idx)[ok]] <- x[idx][ok]
  res
}
