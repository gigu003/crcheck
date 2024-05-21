#' Check if the Chinese ID number complies with the GB 11643-1999 standard.
#'
#' @param x A character vector containing Chinese ID numbers to be checked for
#'          compliance with the GB 11643-1999 standard.
#' @param return A character string used to specific the type of result
#'          returned. Options are 'logical', 'date', 'areacode', 'formatted'.
#'          Options include:
#'          \itemize{
#'            \item{'logical': Logical vector (default). Indicates whether
#'                  each input ID number complies with the standard (TRUE for
#'                  compliant, FALSE for non-compliant).}
#'            \item{'date': Date vector representing birth date contained in
#'                  the ID number.}
#'            \item{'areacode': Character vector containing administrative
#'                  region codes extracted from the ID number.}
#'            \item{'formatted': Character vector containing formatted ID
#'                  numbers.}
#'          }
#'
#'
#' @return A logical vector indicating whether each input ID number complies
#'          with the standard (TRUE for compliant, FALSE for non-compliant),
#'          or other types of information based on the 'return' parameter.
#' @export
#'
#' @examples
#' ids <- c("412726198407240038", "110101199003070975",
#'          "310101199001010101", "440101199102030303")
#' check_id(ids)
#' check_id(ids, return = "date")
#' check_id(ids, return = "areacode")
#' check_id(ids, return = "formatted")
#' 
#' 
check_id <- function(x, return = "logical") {
  # Check the basic format.
  valid_format <- grepl("^\\d{17}[0-9X]$", x)
  s <- x
  #fill the birthyear information.
  d15 <- x[nchar(x) == 15]
  s[nchar(x) == 15] <- paste0(substr(d15, 1, 6), "19", substr(d15, 7, 15))
  
  # Check if the date information in the ID number is valid.
  dates <- as.Date(substr(s, 7, 14), format = "%Y%m%d")
  valid_dates <- !is.na(dates)
  
  last <- nchar(s) >=17 & valid_dates
  
  # Extract the first 17 digits.
  ai <- lapply(strsplit(substr(s[last], 1, 17), ""), as.numeric)
  
  # Calculate the validation code.
  wi <- c(7, 9, 10, 5, 8, 4, 2, 1, 6, 3, 7, 9, 10, 5, 8, 4, 2)
  re <- lapply(ai, function(x) sum(x * wi) %% 11)
  a1 <- c(1, 0, 10, 9, 8, 7, 6, 5, 4, 3, 2)
  cal_a1 <- lapply(re, function(x) a1[x + 1])
  expect <- sub("10", "X", as.character(unlist(cal_a1)))
  
  # Check last character was equal to validation code.
  last_char <- substr(s[last], 18, 18)
  valid_last_char <- expect == last_char
  valid_format[last] <- valid_last_char
  s[last&nchar(s)==17] <- paste0(s[last&nchar(s)==17], expect[last_char==""])
  
  # Validation result.
  res <- valid_format & valid_dates
  s[!((nchar(x)==15 &valid_dates)|valid_format)] <- NA
  if (return == "logical"){
    return(res)
  } else if (return == "date"){
    return(dates)
  } else if (return == "areacode"){
    areacode <- substr(x, 1, 6)
    return(areacode)
  } else if(return == "formatted"){
    return(s)
  } else {
    print("Return_type specified is not supported.")
  }
}
