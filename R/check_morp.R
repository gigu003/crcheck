#' Verify if the Morphology codes for ICDO3 comply with the ICDO3
#' coding rules.
#'
#'
#' @template morp
#' @template quiet
#'  
#' @template return
#' @export
#'
#' @examples
#' morps <- c(9053, 8000, 8170, 8050, 8111)
#' res <- check_morp(morps)
#' print(res)
#' 
check_morp <- function(morp, quiet = FALSE) {
  miss_valid <- is.na(morp)
  form_valid <- !grepl("^\\d{4}$", morp)
  range_valid <- ifelse(!form_valid,
                        unlist(lapply(morp[!form_valid], function(x) {
                          x %nin% ValueRange$iarc$morp
                        })), FALSE)
  #generate result data
  data <- list(morp = morp)
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
  class(res) <- c("check", "morp")
  res$check_item <- "Morphology"
  res$check_standard <- "ICDO3"
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
