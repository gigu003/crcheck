#' Verify character vector comply with gender coding rules
#'
#' Checks whether input values for sex comply with standard coding rules used
#' in tumor registry data. The function verifies format, detects missing or
#' invalid entries, and ensures values fall within the accepted range defined
#' by IARC (e.g., 1 for male, 2 for female, 3 for other/unknown). 
#'
#' @template sex
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' sex <- c(1, 2, 3, 1, 2)
#' res <- check_sex(sex)
#' print(res)
check_sex <- function(sex, quiet = FALSE) {
  form_valid <- !grepl("^[0-9]$", sex)
  miss_valid <- is.na(sex)
  range_valid <- sex %nin% ValueRange$iarc$sex
  #generate result data
  data <- list(sex = sex)
  check <- !Reduce('|', list(miss_valid, form_valid, range_valid))
  type <- ifelse(check, 1, 3)
  error_code <- ifelse(miss_valid, "E-MISS",
                       ifelse(form_valid, "E-FORM",
                              ifelse(range_valid, "E-OUTR", "C")))
  error_desc <- ifelse(miss_valid, "001",
                       ifelse(form_valid, "002",
                              ifelse(range_valid, "003", "0")))
  #output result
  res <- list()
  class(res) <- c("check", "sex", "list")
  res$check_item <- "Sex"
  res$check_standard <- "std"
  res$check_version <- "IARC Check Rules 2005"
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
