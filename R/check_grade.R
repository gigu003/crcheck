check_grade <- function(grade) {
  data <- list(grade = grade)
  grade <- as.integer(grade)
  check <- grade %in% c(1:9)
  type <- ifelse(check, 1, 3)
  message <- ifelse(check, 0, 8)
  res <- list()
  class(res) <- c("check", "grade")
  res$check_item <- "Grade"
  res$check_version <- "IARC Check Rules 2005"
  res$data <- data
  res$check <- check
  res$type <- factor(type,
    levels = c(1:3),
    labels = c("correct", "warning", "error")
  )
  res$message <- message
  print(res)
  invisible(res)
}
