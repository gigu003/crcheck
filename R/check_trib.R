#' Verify codes of nationalities whether comply with GB3304-91 in China
#'
#' @template trib 
#' @template quiet
#'
#' @template return
#' @export
#'
check_trib <- function(trib, quiet = FALSE) {
  form_valid <- !grepl("^\\d{2}$", trib)
  miss_valid <- is.na(trib)
  range_valid <- trib %nin% ValueRange$ncc$trib
  #generate result data
  data <- list(trib = trib)
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
  res$check_item <- "Trib"
  res$check_standard <- "GB3304-91"
  res$check_version <- "1991"
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
