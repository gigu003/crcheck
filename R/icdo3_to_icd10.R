#' Convert ICD-O-3 topography, morphology and behaviour to ICD-10
#'
#' Convert ICD-O-3 topography, morphology and behaviour codes to ICD-10 codes
#' using the package ICD-O-3 to ICD-10 rule set.
#'
#' @param topo A character vector of ICD-O-3 topography codes, such as
#'   `"C50.9"` or `"C509"`.
#' @param morp A character, integer or numeric vector of ICD-O-3 morphology
#'   codes, such as `8500`, `"8500"` or `"8500/3"`.
#' @param beha A character, integer or numeric vector of ICD-O-3 behaviour
#'   codes.
#' @param unmatched A character scalar returned when no rule is matched or
#'   when any of `topo`, `morp` or `beha` is missing. Default is `NA_character_`.
#'
#' @details
#' Matching follows the priority stored in the rule metadata:
#' `topo + morp + beha` first, then `morp + beha`, and finally `topo + beha`.
#' Rule fields containing comma-separated topography or morphology codes are
#' pre-expanded in the package data for faster vectorized matching.
#'
#' @return A character vector of ICD-10 codes with the same length as `topo`,
#'   `morp` and `beha`.
#'
#' @examples
#' icdo3_to_icd10("C50.9", 8500, 3)
#' icdo3_to_icd10(c("C50.9", "C18.0"), c(8500, 8220), c(3, 0))
#'
#' @export
icdo3_to_icd10 <- function(topo, morp, beha, unmatched = NA_character_) {
  check_equal_length(topo, morp, beha, arg_names = c("topo", "morp", "beha"))

  if (!is.character(unmatched) || length(unmatched) != 1L) {
    stop("`unmatched` must be a character scalar.", call. = FALSE)
  }

  topo_std <- std_topo(topo)
  morp_std <- std_morp(morp)
  beha_std <- std_beha(beha)

  res <- rep(unmatched, length(topo_std))
  idx <- !is.na(topo_std) & !is.na(morp_std) & !is.na(beha_std)

  if (!any(idx)) {
    return(res)
  }

  maps <- ICDO3toICD10Rule$maps

  key_tmb <- paste(topo_std[idx], morp_std[idx], beha_std[idx], sep = "|")
  val <- maps$topo_morp_beha[match(key_tmb, names(maps$topo_morp_beha))]

  idx_mb <- is.na(val)
  if (any(idx_mb)) {
    key_mb <- paste(morp_std[idx][idx_mb], beha_std[idx][idx_mb], sep = "|")
    val[idx_mb] <- maps$morp_beha[match(key_mb, names(maps$morp_beha))]
  }

  idx_tb <- is.na(val)
  if (any(idx_tb)) {
    key_tb <- paste(topo_std[idx][idx_tb], beha_std[idx][idx_tb], sep = "|")
    val[idx_tb] <- maps$topo_beha[match(key_tb, names(maps$topo_beha))]
  }

  res[idx] <- ifelse(is.na(val), unmatched, unname(val))
  res
}

#' Convert ICD-10 codes to ICD-O-3 topography codes
#'
#' Convert ICD-10 codes to possible ICD-O-3 topography codes using the
#' package ICD-O-3 to ICD-10 topography and behaviour rule set.
#'
#' @param icd10 A character vector of ICD-10 codes, such as `"C50.9"`,
#'   `"C509"`, `"D12.0"` or `"D120"`.
#' @param sep A character scalar used to separate compressed topography code
#'   groups. Default is `", "`.
#' @param unmatched A character scalar returned when no rule is matched or
#'   when `icd10` is missing. Default is `NA_character_`.
#'
#' @details
#' The conversion is not necessarily one-to-one. One ICD-10 code may map to
#' multiple ICD-O-3 topography codes. Multiple matches are deduplicated,
#' sorted and compressed with [compress_topo()]. If no topography and
#' behaviour rule is matched, ICD-10 codes that are also valid ICD-O-3
#' topography codes are returned as topography codes, with selected ICD-10
#' site-specific fallback mappings applied.
#'
#' @return A character vector with the same length as `icd10`. Each element is
#'   a compressed string of possible ICD-O-3 topography codes.
#'
#' @examples
#' icd10_to_topo("D12.0")
#' icd10_to_topo(c("D10.0", "C50.9"))
#'
#' @export
icd10_to_topo <- function(icd10, sep = ", ", unmatched = NA_character_) {
  #icd10 <- gen_icd10(0:993)
  #sep <- ", "
  #unmatched = NA_character_
  if (!is.character(icd10)) {
    stop("`icd10` must be a character vector.", call. = FALSE)
  }

  if (!is.character(sep) || length(sep) != 1L || is.na(sep)) {
    stop("`sep` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!is.character(unmatched) || length(unmatched) != 1L) {
    stop("`unmatched` must be a character scalar.", call. = FALSE)
  }

  icd10_std <- std_icd10(icd10)
  res <- rep(unmatched, length(icd10_std))
  idx <- !is.na(icd10_std)

  if (!any(idx)) {
    return(res)
  }

  rule <- ICDO3toICD10Rule$topo_beha
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
    80
  )
  special_c <- sprintf("C%02d", special_c)

  res[idx] <- vapply(
    icd10_std[idx],
    function(z) {
      if (z %in% special_c) {
        return(paste0(z, ".9"))
      } else if (z %in% paste0("C22.", c(0, 2, 3, 4, 7, 9))) {
        return("C22.0")
      } else if (z %in% c("C22.1")) {
        return("C22.1")
      } else if (z %in% paste0("C43.", 0:9)) {
        return(gen_topo(as.integer(gsub("\\D", "", z)) + 10))
      } else if (z %in% c("C46.0")) {
        return("C44.9")
      } else if (z %in% c("C46.1")) {
        return("C49.9")
      } else if (z %in% c("C46.2")) {
        return("C05.9")
      } else if (z %in% c("C46.3")) {
        return("C77.9")
      } else if (z %in% c("C46.7")) {
        return("C00-C80")
      } else if (z %in% c("C46.8", "C46.9")) {
        return("C80.9")
      } else if (z %in% c("C45.0")) {
        return("C38.4")
      } else if (z %in% c("C45.1")) {
        return("C48.2")
      } else if (z %in% c("C45.2")) {
        return("C38.0")
      } else if (z %in% c("C45.7")) {
        return("C00-C80")
      } else if (z %in% c("C45.9")) {
        return("C80.9")
      } else if (substr(z, 1, 3) %in% paste0("C", 81:88)) {
        return("C77.9")
      } else if (z %in% c("C90.0", "C90.1")) {
        return("C42.1")
      } else if (z %in% c("C90.2", "C90.3", "C90.9")) {
        return(unmatched)
      } else if (substr(z, 1, 3) %in% paste0("C", 91:95)) {
        return("C42.1")
      } else {
        topo <- rule$topo[rule$icd10 == z]
        topo <- unlist(
          recode_topo(topo, simplify = FALSE, sort_result = TRUE),
          use.names = FALSE
        )
        topo <- sort(unique(topo[!is.na(topo)]))
        compress_topo(topo, sep = sep)
      }
    },
    character(1)
  )
  res
}
