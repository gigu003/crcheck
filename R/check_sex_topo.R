#' Check consistency between sex and site code
#'
#' @template sex
#' @template topo
#' @template std
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' sex <- c(1, 1, 1, 2, 2, 2)
#' topo <- c("C51.0", "C53.0", "C57.4", "C60.0", "C60.9", "C63.9")
#' res <- check_sex_topo(sex, topo)
#' print(res)
check_sex_topo <- function(sex,
                           topo,
                           std = "iacr",
                           quiet = FALSE) {
  topo <- add_decimal(topo)
  if (std %in% c("iacr", "encr")) {
    male <- sex == 1 & topo %in% gen_topo(510:589)
    female <- sex == 2 & topo %in% gen_topo(600:639)
  } else {
    stop("std specified was not allowed.")
  }

  # generate output result
  data <- list(sex = sex, topo = topo)
  check <- !(male | female)
  type <- ifelse(male | female, 3, 1)
  error_code <- ifelse(!check, "E-SETO", "C")
  error_desc <- ifelse(male, 201, ifelse(female, 202, 0))
  # output res
  res <- list()
  class(res) <- c("check", "sex/topo")
  res$check_item <- "Sex/Topo"
  res$check_standard <- std
  res$check_version <- "IARC Tech Report 2005"
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
