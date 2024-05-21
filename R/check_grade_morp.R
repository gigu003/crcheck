check_grade_morp <- function(grade, morp) {
  data <- list(grade = grade, morp = morp)
  morp <- gsub("[^0-9]", "", morp)
  morp <- as.integer(morp)
  grade <- as.integer(grade)
  check2 <- grade %in% c(5:8) & morp < 9590
  check3 <- grade %in% c(1:4) & morp >= 9590
  check4 <- (!grade == 5) &
    morp %in% c(9702, 9705, 9706, 9708, 9709, 9717, 9718, 9729, 9827, 9834, 9837)
  check5 <- grade %nin% c(5, 7) & morp %in% c(9714, 9831)
  check6 <- grade %nin% c(5, 8, 9) & morp %in% gen_morp(9700:9719)
  check7 <- (!grade == 6) &
    morp %in% c(gen_morp(9670:9699), 9728, 9823, 9826, 9833, 9836)
  check8 <- (!grade == 8) & morp == 9948
  check9 <- (!grade == 1) & morp %in% c(8331, 9187, 9511)
  check10 <- (!grade == 2) &
    morp %in% c(8332, 8858, 9083, 9243, 9372)
  check11 <- (!grade == 3) & morp %in% c(8631, 8634)
  check12 <- (!grade == 4) &
    morp %in% c(8020, 8021, 8805, 9062, 9082, 9390, 9392, 9401, 9451, 9505, 9512)
  check <- check2 | check3 | check4 | check5 | check6 | check7 |
    check8 | check9 | check10 | check11 | check12
  check <- !check
  type <- ifelse(check, 1, 2)
  message <- ifelse(check2, 212, ifelse(check3, 213, ifelse(check4, 214,
                    ifelse(check5, 215, ifelse(check6, 216,
                           ifelse(check7, 217, ifelse(check8, 218,
                                  ifelse(check9, 219, ifelse(check10, 220,
                                  ifelse(check11, 221,ifelse(check12, 222,0
                                                             )))))))))))
  res <- list()
  class(res) <- c("check", "grade/morp")
  res$check_item <- "Grade/Morp"
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