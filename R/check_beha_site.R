check_beha_site <- function(beha, site) {
  beha <- as.integer(beha)
  beha_site <- beha == 2 & site %in% gen_topo(c(400:429,470:479,490:499,
                                                     700:729))
  check <- ! beha_site
  type <- ifelse(check, 1, 2)
  message <- ifelse(beha_site, 201, 0)
  data <- list(beha = beha, site = site)
  res <- list()
  class(res) <- c("check","beha/site")
  res$check_item <- "Behaviour/Site"
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