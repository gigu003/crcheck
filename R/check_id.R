#' Check whether ID numbers comply with the GB11643-1999 in China.
#'
#' This function validates Chinese citizen ID numbers based on the national
#' standard GB 11643-1999. It checks the overall format, verifies the birth
#' date portion, and validates the final check digit using the official
#' algorithm.
#'
#' @param x A character vector containing Chinese ID numbers.
#' @template quiet
#' 
#' @template return
#' @export
#'
#' @examples
#' ids <- c(
#'   "412726198407240038", "110101199003070975",
#'   "310101199001010101", "440101199102030303"
#' )
#' res <- check_id(ids)
#' print(res)
#' 
check_id <- function(x, quiet = FALSE) {
  # Check the basic format.
  valid_format <- !grepl("^\\d{17}[0-9X]$", x)
  valid_dates <- ifelse(!valid_format,
                        unlist(lapply(x[!valid_format], function(x) {
                          is.na(as.Date(substr(x, 7, 14), format = "%Y%m%d"))
                        })), FALSE)
  loc <- !valid_format & !valid_dates
  valid_last_char <- ifelse(!valid_format & !valid_dates,
                            unlist(lapply(x[loc], last_valid)),
                            FALSE)
  #generate result data
  data <- list(id = x)
  check <- !Reduce('|', list(valid_format, valid_dates, valid_last_char))
  type <- ifelse(check, 1, 3)
  error_code <- ifelse(is.na(x), "E-MISS",
                       ifelse(valid_format, "E-FORM",
                              ifelse(!check, "E-OUTR", "C")))
  groups <- ifelse(is.na(x), "001",
                   ifelse(valid_format, "002",
                          ifelse(valid_dates, "004",
                                 ifelse(valid_last_char, "005", "0"))))
  error_desc <- groups
  #Output result
  res <- list()
  class(res) <- c("check", "id")
  res$check_item <- "ID"
  res$check_standard <- "GB 11643-1999"
  res$check_version <- "GB 11643-1999"
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

last_valid <- function(x) {
  ai <- as.numeric(unlist(strsplit(substr(x, 1, 17), "")))
  wi <- c(7, 9, 10, 5, 8, 4, 2, 1, 6, 3, 7, 9, 10, 5, 8, 4, 2)
  re <- sum(ai * wi) %% 11
  a1 <- c(1, 0, 10, 9, 8, 7, 6, 5, 4, 3, 2)
  cal_a1 <- a1[re + 1]
  expect <- sub("10", "X", as.character(cal_a1))
  last_char <- substr(x, 18, 18)
  !last_char == expect
}
