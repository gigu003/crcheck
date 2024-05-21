check_sex_topo <- function(sex, topo) {
  male <- sex == 1 & substr(topo, 1, 3) %in% paste0("C", c(51:58))
  female <- sex == 2 & substr(topo, 1, 3) %in% paste0("C", c(69:63))
  check <- !male & !female
  type <- ifelse( male | female, 3, 1)
  message <- ifelse(male, 181, ifelse(female, 182, 0))
  res <- list()
  class(res) <- c("check","sex/topo")
  res$check_item <- "Sex/Site"
  res$check_version <- "IARC Tech Report 2005"
  res$check <- check
  res$type <- type
  res$message <- message
  print(res)
  invisible(res)
}