#' Check consistency between birth date incidence and Date of last known vital status
#'
#' @template birthda
#' @template inciden
#' @template lastcontact
#' @template quiet
#'
#' @template return
#' @export
#'
#'
check_birth_inci_last <- function(birthda,
                                  inciden,
                                  lastcontact,
                                  quiet = FALSE) {
  nmiss <- !is.na(birthda) & !is.na(inciden) & !is.na(lastcontact)
  coda <- nmiss & birthda > inciden
  codv <- nmiss & (birthda > lastcontact | inciden > lastcontact)
  #generate result data
  data <- list(birthda = birthda,
               inciden = inciden,
               lastcontact = lastcontact)
  check <- !Reduce('|', list(coda, codv))
  type <- ifelse(check, 1, 3)
  error_code <- ifelse(coda, "E-CoDA",
                       ifelse(codv, "E-CoDV", "C"))
  error_desc <- ifelse(coda, 201,
                       ifelse(codv, 202, 0))
  #output the result
  res <- list()
  class(res) <- c("check", "birth/inci/last")
  res$check_item <- "Birthda/Inciden/Lastcontact"
  res$check_standard <- "ENCR"
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
