#' Generate valid ICDO3 topography codes.
#'
#' @param x Number between 0 and 809
#' @param na_drop Logical value indicates whether drop NA values or not.
#' @param morp Optional vector of morphology codes. If provided, only return topography
#'     codes that are suggested by the given morphology codes. Default is `NULL`.
#'
#' @return A vector of valid ICDO3 topography codes.
#' @export
#'
#' @examples
#' gen_topo(160:169)
#' gen_topo(170:179)
#' gen_topo(330:349)
gen_topo <- function(x = NULL, na_drop = TRUE, morp = NULL) {
  if (is.null(x) && !is.null(morp)) {
    x <- 0:809
  }

  topo_number <- as.numeric(gsub("[^0-9]", "", ValueRange$topo$range))
  x <- ifelse(!x %in% topo_number, NA, x)
  if (na_drop) {
    x <- x[!is.na(x)]
  }

  res <- ifelse(x < 100, sprintf("C0%.1f", x / 10), sprintf("C%.1f", x / 10))
  if (!is.null(morp)) {
    suggest_topo <- vapply(
      morp,
      function(i) {
        idx <- which(
          vapply(
            morp_suggest_topo[["morp"]],
            function(z) i %in% z,
            logical(1)
          )
        )
        if (length(idx) == 0) {
          NA_character_
        } else {
          compress_topo(morp_suggest_topo[["topo"]][[idx]], sep = ", ")
        }
      },
      character(1)
    )
    return(suggest_topo)
  }
  res
}

#' Generate ICD-10 codes from numeric ranges
#'
#' Convert numeric values to ICD-10 topography-style codes
#' (e.g. `180 -> "C18.0"`).
#'
#' @param x Numeric or integer vector.
#' @param init Character scalar. Prefix of ICD-10 code.
#'   Must be either `"C"` or `"D"`.
#' @param na_drop Logical. If `TRUE`, remove invalid values converted to `NA`.
#' @param not_icdo3 Logical. If `TRUE` and `init = "C"`,
#'   only return codes that are not included in ICD-O-3 topography codes.
#' @param special Logical. If `TRUE`, only keep speical ICD10 codes.
#' @return A character vector of ICD-10 codes.
#'
#' @export
#'
#' @examples
#' gen_icd10(180:229)
#' gen_icd10(220:229)
gen_icd10 <- function(
  x,
  init = c("C", "D"),
  na_drop = TRUE,
  not_icdo3 = FALSE,
  special = FALSE
) {
  init <- match.arg(init)
  special_c <- c(
    1,
    7,
    12,
    19,
    20,
    23,
    33,
    37,
    52,
    55,
    56,
    58,
    61,
    64,
    65,
    66,
    73,
    80,
    97
  )
  special_d <- c(24, 27, 34, 45)
  if (init == "C") {
    x <- ifelse(x %in% ValueRange$not_icd10$C | x > 979, NA, x)
    if (na_drop) {
      x <- x[!is.na(x)]
    }
    res <- ifelse(
      x %in% (special_c * 10),
      sprintf("C%02d", floor(x / 10)),
      ifelse(x < 100, sprintf("C0%.1f", x / 10), sprintf("C%.1f", x / 10))
    )
  } else if (init == "D") {
    x <- ifelse(x %in% ValueRange$not_icd10$D | x > 488, NA, x)
    if (na_drop) {
      x <- x[!is.na(x)]
    }
    res <- ifelse(
      x %in% (special_d * 10),
      sprintf("D%02d", floor(x / 10)),
      ifelse(x < 100, sprintf("D0%.1f", x / 10), sprintf("D%.1f", x / 10))
    )
  }

  if (not_icdo3 & init == "C") {
    res <- res[
      res %nin%
        gen_topo(x) &
        res %nin% sprintf("C%02d", special_c[-length(special_c)])
    ]
  }

  if (special) {
    res <- res[
      res %in% c(sprintf("C%02d", special_c), sprintf("D%02d", special_d))
    ]
  }
  return(res)
}

#' Generate valid ICDO3 morphology codes from range of numbers.
#'
#' @param x Numbers
#' @param na_drop Logical value indicates whether drop NA values or not.
#' @param show_beha Logical. If `TRUE`, return valid ICD-O-3 morphology
#'   codes with corresponding behaviour codes in `"morp/beha"` format
#'   (e.g., `"8440/3"`). The mapping is based on `IARC_BehaMorpRule`.
#'   A morphology code may map to multiple behaviour codes, in which
#'   case multiple entries are returned. Default is `FALSE`.
#'
#' @return Vector of valid ICDO3 morphology codes. When `show_beha = TRUE`,
#'   returns a character vector in `"morp/beha"` format.
#' @export
#'
#' @examples
#' gen_morp(8000:8090)
#' gen_morp(8500:8700)
#' gen_morp(8440, show_beha = TRUE)
gen_morp <- function(x, na_drop = TRUE, show_beha = FALSE) {
  res <- ifelse(x %nin% ValueRange$morp$range, NA, x)
  if (na_drop) {
    res <- res[!is.na(res)]
  }

  if (show_beha) {
    beha_names <- names(IARC_BehaMorpRule)
    m <- do.call(cbind, lapply(IARC_BehaMorpRule, function(r) res %in% r))
    idx <- which(m, arr.ind = TRUE)
    res <- sprintf("%04d/%s", res[idx[, "row"]], beha_names[idx[, "col"]])
  }

  return(res)
}
