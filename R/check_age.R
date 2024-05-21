check_age <- function(age,
                      birth_date = NULL,
                      onset_date = NULL) {
  data <- list(age = age, birth_date = birth_date, onset_date = onset_date)
  check1 <- age > 105 | age < 0
  if (!is.null(birth_date) & !is.null(onset_date)) {
    age_cal <- calc_age(birth_date, onset_date)
    check2 <- !age == age_cal
    check <- check1 | check2
    type <- ifelse(check1, 2, ifelse(check2, 3, 1))
    message <- ifelse(check1, 1, ifelse(check2, 2, 0))
  } else {
    check <- check1
    type <- ifelse(check, 2, 1)
    message <- ifelse(check, 1, 0)
  }
  res <- list()
  class(res) <- c("check","age/birthdate/onsetdate")
  res$check_item <- "Age/Birth date/Onset date"
  res$check_version <- "IARC Check Rules 2005"
  res$data <- data
  res$check <- check
  res$type <- factor(type,
                     levels = c(1:3),
                     labels = c("correct", "warning", "error"))
  res$message <- message
  print(res)
  invisible(res)
}

calc_age <- function(birth_date, onset_date) {
  birth_date <- as.Date(birth_date)
  onset_date <- as.Date(onset_date)
  birth_year <- as.numeric(format(birth_date, "%Y"))
  onset_year <- as.numeric(format(onset_date, "%Y"))
  age <- onset_year - birth_year
  birth_month <- as.numeric(format(birth_date, "%m"))
  onset_month <- as.numeric(format(onset_date, "%m"))
  birth_day <- as.numeric(format(birth_date, "%d"))
  onset_day <- as.numeric(format(onset_date, "%d"))
  age[onset_month < birth_month |
        (onset_month == birth_month &
           onset_day < birth_day)] <-
    age[onset_month < birth_month |
          (onset_month == birth_month &
             onset_day < birth_day)] - 1
  return(age)
}
