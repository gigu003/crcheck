#' Check the Basis of diagnosis code 
#'
#' @template basi
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' basi <- c(1, 2, 4, 5, 6, 7, 9, 3)
#' res <- check_basi(basi)
#' print(res)
check_basi <- function(basi, quiet = FALSE) {
  miss_valid <- is.na(basi)
  form_valid <- !grepl("^\\d{1}$", basi)
  range_valid <- ifelse(!form_valid,
                        unlist(lapply(basi[!form_valid], function(x) {
                          x %nin% ValueRange$iarc$basi
                        })), FALSE)
  #generate result data
  data <- list(basi = basi)
  check <- !Reduce('|', list(miss_valid, form_valid, range_valid))
  type <- ifelse(check, 1, 3)
  error_code <- ifelse(miss_valid, "E-MISS",
                       ifelse(form_valid, "E-FORM",
                              ifelse(range_valid, "E-OUTR", "C"))
  )
  error_desc <- ifelse(miss_valid, "001",
                       ifelse(form_valid, "002",
                              ifelse(range_valid, "003", "0")))
  # output result
  res <- list()
  class(res) <- c("check", "basi")
  res$check_item <- "Basi"
  res$check_standard <- "IARC"
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
