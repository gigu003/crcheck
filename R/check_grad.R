#' Check grade codes
#'
#'
#' @template grad
#' @template quiet
#'
#' @template return
#' @export
#' 
#' @examples
#' 
#' grads <- c(1, 2, 3, 9, 5, 8)
#' res <- check_grad(grads)
#' print(res)
check_grad <- function(grad, quiet = FALSE) {
  miss_valid <- is.na(grad)
  form_valid <- !grepl("^\\d{1}$", grad)
  range_valid <- ifelse(!form_valid,
                        unlist(lapply(grad[!form_valid], function(x) {
                          x %nin% ValueRange$iarc$grad
                        })), FALSE)
  #generate result data
  data <- list(grad = grad)
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
  class(res) <- c("check", "grad")
  res$check_item <- "Grad"
  res$check_standard <- "ICDO3.2"
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
