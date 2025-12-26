#' Generate valid ICDO3 topography codes.
#'
#' @param x Number between 0 and 809
#' @param na_drop Logical value indicates whether drop NA values or not.
#'
#' @return A vector of valid ICDO3 topography codes.
#' @export
#'
#' @examples
#' gen_topo(160:169)
#' gen_topo(170:179)
#' gen_topo(330:349)
gen_topo <- function(x, na_drop = TRUE) {
  topo_number <- as.numeric(gsub("[^0-9]", "", ValueRange$iarc$topo))
  x <- ifelse(!x %in% topo_number, NA, x)
  if (na_drop) {
    x <- x[!is.na(x)]
  }
  res <- ifelse(x < 100,
                sprintf("C0%.1f", x / 10),
                sprintf("C%.1f", x / 10)
  )
  return(res)
}

#' Generate valid ICD10 codes from ranges of numbers
#'
#' @param x Numbers
#' @param init Initial letter, "C" or "D"
#' @param na_drop Logical value indicates whether drop NA values or not.
#'
#' @return Vector of ICD10 codes.
#' @export
#'
#' @examples
#' gen_icd10(180:229)
#' gen_icd10(220:229)
gen_icd10 <- function(x, init = "C", na_drop = TRUE) {
  if (init == "C") {
    x <- ifelse(x %in% ValueRange$not_icd10$C | x > 979, NA, x)
    if (na_drop) {
      x <- x[!is.na(x)]
    }
    res <- ifelse(x %in% (c(1, 7, 12, 19, 20, 23, 33, 37, 52, 55, 56,
                            58, 61, 64, 65, 66, 73, 97) * 10),
                  sprintf("C%02d", floor(x / 10)),
                  ifelse(x < 100,
                         sprintf("C0%.1f", x / 10),
                         sprintf("C%.1f", x / 10)))
  } else if (init == "D") {
    x <- ifelse(x %in% ValueRange$not_icd10$D | x > 488, NA, x)
    if (na_drop) {
      x <- x[!is.na(x)]
    }
    res <- ifelse(x %in% (c(24, 27, 34, 45) * 10),
                  sprintf("D%02d", floor(x / 10)),
                  ifelse(x < 100, sprintf("D0%.1f", x / 10),
                         sprintf("D%.1f", x / 10)
                  )
    )
  }
  return(res)
}

#' Generate valid ICDO3 morphology codes from range of numbers.
#'
#' @param x Numbers
#' @param na_drop Logical value indicates whether drop NA values or not.
#'
#' @return Vector of valid ICDO3 morphology codes.
#' @export
#'
#' @examples
#' gen_morp(8000:8090)
#' gen_morp(8500:8700)
gen_morp <- function(x, na_drop = TRUE) {
  res <- ifelse(x %nin% ValueRange$iarc$morp, NA, x)
  if (na_drop) {
    res <- res[!is.na(res)]
  }
  return(res)
}
