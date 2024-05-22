#' Verify if the topographic site codes for ICDO3 comply with the ICDO3
#' coding rules.
#'
#' @param x Character vector contains the topographic site codes for ICDO3.
#' @return check_topo returns an object of class of 'check'.
#' @export
#'
#' @examples
#' topos <- c("C50.9", "16.2", "C151", "33.2")
#' check_topo(topos)
check_topo <- function(x) {
  topo_number <- gsub("[^0-9]", "", x)
  full <- sprintf("C%03s", topo_number)
  init_letter <- substr(x, 1, 1) == "C"

  islegal <- full %in% topo_dict
  check <- islegal & init_letter
  type <- ifelse((!toupper(substr(x, 1, 1)) == "C") | (!islegal),
    3,
    ifelse(check, 1, 2)
  )
  message <- ifelse(!init_letter, 4, ifelse(!islegal, 5, 0))
  full <- paste0(substr(full, 1, 3), ".", substr(full, 4, 4))
  full <- ifelse(!toupper(substr(x, 1, 1)) == "C",
    NA, full
  )
  res <- list()
  class(res) <- c("check", "topo")
  res$check_item <- "Site"
  res$check_version <- "ICDO3.2"
  res$data <- x
  res$check <- islegal & init_letter
  res$type <- factor(type,
    levels = c(1:3),
    labels = c("correct", "warning", "error")
  )
  res$message <- message
  res$correct <- full
  print(res)
  invisible(res)
}

#' generate legal ICDO3 Topo code
#'
#' @param x Number between 0 and 809
#' @param na_drop Drop illegal code or not.
#'
#' @return A vector of legeal TOPO codes.
#' @export
#'
#' @examples
#' gen_topo(1:169)
gen_topo <- function(x, na_drop = TRUE) {
  topo_number <- as.numeric(gsub("[^0-9]", "", topo_dict))
  x <- ifelse(!x %in% topo_number, NA, x)
  if (na_drop) {
    x <- x[!is.na(x)]
  }
  res <- ifelse(x < 100,
    sprintf("C0%.1f", x / 10),
    sprintf("C%.1f", x / 10)
  )
  return(res)
}
