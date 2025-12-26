#' Check the ICD10 code in the tumor part.
#'
#' @template icd10
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' icd10 <- c("C15.2", "C17.0", "C19.0", "C34.9", "D15.2", "c15.2")
#' res <- check_icd10(icd10)
#' print(res)
check_icd10 <- function(icd10, quiet = TRUE) {
  miss_valid <- is.na(icd10)
  form_valid <- !valid_icd10(icd10)
  range_valid <- unlist(lapply(icd10, range_check))
  #generate result data
  data <- list(icd10 = icd10)
  check <- !Reduce('|', list(miss_valid, form_valid, range_valid))
  type <- ifelse(check, 1, 3)
  error_code <- ifelse(miss_valid, "E-MISS",
                       ifelse(form_valid, "E-FORM",
                              ifelse(range_valid, "E-OUTR", "C"))
  )
  error_desc <- ifelse(miss_valid, "001",
                       ifelse(form_valid, "002",
                              ifelse(range_valid, "003", "0")))
  #output result
  res <- list()
  class(res) <- c("check", "icd10")
  res$check_item <- "ICD10"
  res$check_standard <- "ICD10"
  res$check_version <- "ICD102019"
  res$data <- data
  res$check <- check
  res$type <- type
  res$error_code <- error_code
  res$error_desc <- error_desc
  if (!quiet) {
    summary(res)  
  }
  invisible(res)
}


valid_icd10 <- function(icd10) {
  special_c <- c(1, 7, 12, 19, 20, 23, 33, 37, 52, 55, 56, 58, 61, 64,
                 65, 66, 73, 97)
  special_d <- c(24, 27, 34, 45)
  special_icd10 <- c(sprintf("C%02d", special_c), paste0("D", special_d))
  special2 <- icd10 %nin% paste0(special_icd10,".0")
  regular <- grepl("^[CD]\\d{2}\\.\\d$", icd10)
  check <- ifelse(icd10 %in% special_icd10, TRUE,
                  ifelse(regular & special2, TRUE, FALSE))
  return(check)
}

range_check <- function(x) {
  init <- toupper(substring(x, 1, 1))
  icdn <- as.numeric(gsub("[^0-9\\.]", "", x))
  if (init == "D") {
    ress <- gen_icd10((icdn * 10), "D", na_drop = FALSE)
  } else if (init == "C") {
    ress <- gen_icd10((icdn * 10), na_drop = FALSE)
  }
  check <- !(ress == x)
  check <- ifelse(is.na(check), TRUE, FALSE)
  return(check)
}