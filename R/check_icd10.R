#' Check the ICD10 code in the tumor part.
#'
#' @param icd10 The ICD10 codes.
#'
#' @return check_icd10 returns an object of class of 'check'.
#'
#' @export
#'
#' @examples
#'
#' icd10 <- c("C15.2", "C17.0", "C19.0", "C34.9", "D15.2")
#' res <- check_icd10(icd10)
check_icd10 <- function(icd10) {
  init_letter <- substring(icd10, 1, 1)
  icd_number <- as.numeric(gsub("[^0-9\\.]", "", icd10))
  ress <- ifelse(
    toupper(init_letter) == "D",
    gen_icd10(icd_number * 10, "D"),
    ifelse(toupper(init_letter) == "C", gen_icd10(icd_number * 10), NA)
  )
  check <- ress == icd10
  check <- ifelse(is.na(check), FALSE, check)

  range_check <- ifelse(
    toupper(init_letter) == "C",
    !icd_number %in% (not_icd10$C / 10) &
      icd_number >= 0 &
      icd_number <= 97,
    ifelse(
      toupper(init_letter) == "D",
      (!icd_number %in% (not_icd10$D / 10)) &
        icd_number >= 0 &
        icd_number <= 488,
      NA
    )
  )

  init_letter_logi <- init_letter %in% c("c", "d")
  type <- ifelse(is.na(ress), 3, ifelse(init_letter_logi, 2, 1))

  message <- ifelse(!init_letter %in% c("C", "D"), 1,
    ifelse(!range_check, 2, 0)
  )
  res <- list()
  class(res) <- c("check", "icd10")
  res$check_item <- "ICD10"
  res$check_version <- "ICD10 2019"
  res$data <- icd10
  res$check <- check
  res$type <- factor(type,
    levels = c(1:3),
    labels = c("correct", "warning", "error")
  )
  res$message <- message
  res$correct <- ress
  print(res)
  invisible(res)
}


valid_icd10 <- function(icd10) {
  special_c <- c(1, 7, 12, 19, 20, 23, 33, 37, 52, 55, 56, 58, 61, 64,
                 65, 66, 73, 97)
  special_d <- c(24, 27, 34, 45)
  special_icd10 <- c(sprintf("C%02d", special_c), paste0("D", special_d))
  pattern <- "^[CD]\\d{2}\\.\\d$"
  res <- grepl(pattern, icd10) | icd10 %in% special_icd10
  return(res)
}


gen_icd10 <- function(x, init = "C", na_drop = FALSE) {
  if (init == "C") {
    x <- ifelse(x %in% not_icd10$C | x > 979, NA, x)
    if (na_drop) {
      x <- x[!is.na(x)]
    }
    res <- ifelse(x %in% c(1, 7, 12, 19, 20, 23, 33, 37, 52, 55, 56,
                           58, 61, 64, 65, 66, 73, 97) * 10,
                  sprintf("C%02d", floor(x / 10)),
                  ifelse(x < 100,
                         sprintf("C0%.1f", x / 10),
                         sprintf("C%.1f", x / 10)))
  } else if (init == "D") {
    x <- ifelse(x %in% not_icd10$D | x > 488, NA, x)
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
