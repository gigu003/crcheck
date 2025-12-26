#' Verify whether characters comply with ICD-O-3 topography coding rules
#'
#' `check_topo()` verifies whether a character vector contains valid ICD-O-3 
#' topography codes, which represent anatomical cancer sites (e.g., "C50.9").
#' It checks each code for correct format, valid value range, and presence of 
#' missing values. Codes with formatting issues (e.g., missing decimal points 
#' or incorrect patterns) or values outside the acceptable range are flagged.
#'
#' For detailed coding rules, refer to the official ICD-O-3 guidelines: 
#' \url{http://www.iacr.com.fr/index.php?Itemid=577}.
#'
#' @template topo
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' topos <- c("C50.9", "16.2", "C151", "33.2")
#' res <- check_topo(topos)
#' print(res)
#'
#' topos <- c("C34.0", "C16.2", "C151", NA)
#' res <- check_topo(topos, quiet = TRUE)
#' print(res)
check_topo <- function(topo, quiet = FALSE) {
  topo <- add_decimal(topo)
  form_valid <- !grepl("^C\\d{2}\\.\\d$", topo)
  range_valid <- ifelse(!form_valid,
                        unlist(lapply(topo[!form_valid], function(x) {
                          topo_number <- gsub("[^0-9]", "", x)
                          full <- sprintf("C%03s", topo_number)
                          full %nin% ValueRange$iarc$topo
                          })), FALSE)
  miss_valid <- is.na(topo)
  #generate result data
  data <- list(topo = topo)
  check <- !Reduce('|', list(form_valid, range_valid, miss_valid))
  type <- ifelse(check, 1, 3)
  error_code <- ifelse(miss_valid, "E-MISS",
                       ifelse(form_valid, "E-FORM",
                              ifelse(range_valid, "E-OUTR", "C"))
                       )
  error_desc <- ifelse(miss_valid, "001",
                       ifelse(form_valid, "002",
                              ifelse(range_valid, "003", "0")))
  #output result
  res <- list()
  class(res) <- c("check", "topo")
  res$check_item <- "Topography"
  res$check_standard <- "ICDO3"
  res$check_version <- "ICDO3.2"
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

add_decimal <- function(x) {
  ifelse(grepl("^C[0-9]{3}$", x),
         sub("([0-9]{2})([0-9])$", "\\1.\\2", x),
         x)
}
