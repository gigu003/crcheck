#' Check consistency between sex and morphology code
#'
#' @template sex
#' @template morp
#' @template std
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' sex <- c(1, 2, 1, 1)
#' morp <- c(8950, 9061, 8600, 9300)
#' res <- check_sex_morp(sex, morp)
#' print(res)
check_sex_morp <- function(sex,
                           morp,
                           std = "iacr",
                           quiet = FALSE
                           ) {
  if (std == "encr") {
    male <- sex == 1 & morp %in% ENCR_SexMorpRule[[1]]
    female <- sex == 2 & morp %in% ENCR_SexMorpRule[[2]]
  } else if (std == "iacr") {
    male <- sex == 1 & morp %in% unlist(IARC_TopoMorpRule[[1]][23:27])
    female <- sex == 2 & morp %in% unlist(IARC_TopoMorpRule[[1]][28:29])
  }
  # generate the output data
  data <- list(sex = sex, morp = morp)
  check <- !(male | female)
  type <- ifelse(male | female, 2, 1)
  error_code <- ifelse(!check, "W-SEMO", "C")
  error_desc <- ifelse(male, 211, ifelse(female, 212, 0))
  # output the result
  res <- list()
  class(res) <- c("check", "sex/morp")
  res$check_item <- "Sex/Histology"
  res$check_standard <- std
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
