#' Check consistency between age and dates.
#'
#' @template age
#' @template birthda
#' @template inciden
#' @template quiet
#' 
#' @template return
#' @export
#'
#' @examples
#' age <- c(58, 59, 60)
#' birth <- c("1960-08-24", "1956-09-24", "1971-06-30")
#' onset <- c("2024-09-10", "2024-08-10", "2023-09-11")
#' res <- check_age_birth_inci(age, birth, onset)
#' print(res)
check_age_birth_inci <- function(age,
                                 birthda = NULL,
                                 inciden = NULL,
                                 quiet = FALSE) {
  miss_valid <- is.na(age) | is.na(birthda) | is.na(inciden)
  range_valid <- age > 105 | age < 0
  form_valid <- rep(!is.numeric(age), length(age))
  age_cal <- calc_age(birthda, inciden)
  check1 <- ifelse(!miss_valid, abs(age - age_cal) > 1, FALSE)
  #generate result data
  data <- list(age = age, birthda = birthda, inciden = inciden)
  check <- !Reduce('|', list(miss_valid, form_valid, range_valid, check1))
  type <- ifelse(check, 1, 3)
  error_code <- ifelse(miss_valid, "E-AGEC",
                       ifelse(form_valid, "E-FORM",
                              ifelse(range_valid, "E-OUTR",
                                     ifelse(check1, "E-AGED", "C")))
  )
  error_desc <- ifelse(miss_valid, 101,
                       ifelse(form_valid, 102,
                              ifelse(range_valid, 103,
                                     ifelse(check1, 104, 0))))
  #output result
  res <- list()
  class(res) <- c("check", "age/birthda/inciden")
  res$check_item <- "Age/Birthda/Inciden"
  res$check_standard <- "encr"
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

calc_age <- function(start, end) {
  start <- as.Date(start)
  end <- as.Date(end)
  age <- rep(NA, length(start))
  v_dates <- !is.na(start) & !is.na(end)
  # Extract year, month, and day components for valid indices
  syear <- as.numeric(format(start[v_dates], "%Y"))
  eyear <- as.numeric(format(end[v_dates], "%Y"))
  smonth <- as.numeric(format(start[v_dates], "%m"))
  emonth <- as.numeric(format(end[v_dates], "%m"))
  sday <- as.numeric(format(start[v_dates], "%d"))
  eday <- as.numeric(format(end[v_dates], "%d"))
  # Calculate initial age for valid dates
  age[v_dates] <- eyear - syear
  adjustment <- (emonth < smonth) | (emonth == smonth & eday < sday)
  age[v_dates][adjustment] <- age[v_dates][adjustment] - 1
  return(age)
}
