#' Verify if the Behavior codes for ICDO3 comply with the ICDO3 coding rules.
#'
#' @template beha
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' behas <- c(0, 1, 2, 3, 9, 6, 8)
#' res <- check_beha(behas)
#' print(res)
check_beha <- function(beha, quiet = FALSE) {
  miss_valid <- is.na(beha)
  form_valid <- !grepl("^\\d{1}$", beha)
  range_valid <- ifelse(!form_valid,
                        unlist(lapply(beha[!form_valid], function(x) {
                          x %nin% ValueRange$iarc$beha
                        })), FALSE)
  #generate result data
  data <- list(beha = beha)
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
  res$check_item <- "Behaviour"
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
