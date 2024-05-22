#' Check validation of sex code
#'
#' @param sex sex code
#'
#' @return class of 'check'
#' @export
#'
#' @examples
#' sex <- c(1, 2, 3, 1, 2)
#' check_sex(sex)
check_sex <- function(sex) {
  sex <- as.integer(sex)
  check <- sex %in% c(1, 2, 9)
  type <- ifelse(check, 1, 3)
  message <- ifelse(check, 0, 3)
  res <- list()
  class(res) <- c("check", "age/birthdate/onsetdate")
  res$check_item <- "Sex / Birth date / Incidence date"
  res$check_version <- "IARC Check Rules 2005"
  res$data <- sex
  res$check <- check
  res$type <- factor(type,
    levels = c(1:3),
    labels = c("correct", "warning", "error")
  )
  res$message <- message
  print(res)
  invisible(res)
}
