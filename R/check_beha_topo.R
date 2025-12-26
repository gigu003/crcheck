#' Check consistency between behavior and site code
#'
#' @template beha
#' @template topo
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' beha <- c(2, 2, 2, 2)
#' site <- c("C40.9", "C49.1", "C70.0", "C72.9")
#' res <- check_beha_topo(beha, site)
#' print(res)
check_beha_topo <- function(beha, topo, quiet = FALSE) {
  beha <- as.integer(beha)
  topo <- add_decimal(topo)
  beha_site <- beha == 2 & topo %in% gen_topo(c(
    400:429, 470:479, 490:499,
    700:729
  ))
  check <- !beha_site
  # prepare result data
  data <- list(beha = beha, topo = topo)
  type <- ifelse(check, 1, 2)
  error_code <- ifelse(check, "C", "W-TOBE")
  error_desc <- ifelse(beha_site, 201, 0)
  # output result
  res <- list()
  class(res) <- c("check", "beha/topo")
  res$check_item <- "Behaviour/Topo"
  res$check_standard <- "IARC"
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
