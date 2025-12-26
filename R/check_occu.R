#' Verify codes of occupation whether comply with GB/T2261.4 in China
#'
#' @template occu 
#' @template quiet
#'
#' @template return
#' @export
#'
check_occu <- function(occu, quiet = FALSE) {
  form_valid <- !grepl("^\\d{2}$", occu)
  miss_valid <- is.na(occu)
  range_valid <- occu %nin% ValueRange$ncc$occu
  #generate result data
  data <- list(occu = occu)
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
  class(res) <- c("check", "list")
  res$check_item <- "occu"
  res$check_standard <- "GB/T2261.4"
  res$check_version <- "2003"
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
