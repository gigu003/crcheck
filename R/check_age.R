#' Check the validation of age
#'
#' @template age
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' age <- c(0, 1, 135, NA)
#' res <- check_age(age)
#' print(res)
check_age <- function(age, quiet = FALSE) {
  miss_valid <- is.na(age)
  form_valid <- !is.numeric(age)
  range_valid <- as.numeric(age) %nin% ValueRange$age
  #generate result data
  data <- list(age = age)
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
  class(res) <- c("check", "age")
  res$check_item <- "Age"
  res$check_standard <- "ENCR"
  res$check_version <- "ICDO3.2"
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
