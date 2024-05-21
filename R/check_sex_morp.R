check_sex_morp <- function(sex = c(1, 2),
                           morp = c(8905, 8080)) {
  male <- sex == 1 & morp %in% unlist(check_site_morp[[1]][23:27])
  female <- sex == 2 & morp %in% unlist(check_site_morp[[1]][28:29])
  check <- !(male | female)
  type <- ifelse(check == TRUE, 1, 2)
  data <- list(sex = sex, morp = morp)
  message <- ifelse(male, 191, ifelse(female, 192, 0))
  res <- list()
  class(res) <- c("check","sex/morp")
  res$check_item <- "Sex/Histology"
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