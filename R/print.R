#' Show the summary of the check result
#'
#' @param object check class
#' @param ... Other parameters
#'
#' @return summary
#' @export
#'
summary.check <- function(object, ...) {
  cat("Check Items:", object$check_item, "\n")
  cat("Check Standard:", object$check_standard, "\n")
  cat("Standard Version:", object$check_version, "\n")
  cat("Check Results:")
  print(table(factor(object$type,
                     levels = c(1:3),
                     labels = c("Correct", "Warning", "Error"))))
  cat("\n")
}

#' Print list of check result
#'
#' @param x Object of class 'check'
#' @param ... Other parameters.
#'
#' @return A data frame.
#' @export
#'
print.check <- function(x, ...) {
  loc <- which(!x$check)
  data <- lapply(x$data, function(x) x[loc])
  if (length(data) == 1){
    values <- data[[1]]
  } else {
    values <- do.call(rbind, data)
    values <- apply(values, 2, paste, collapse="/")
  }

  res <- data.frame(
    id = loc,
    item = rep(x$check_item, length(loc)),
    value = values,
    type = factor(x$type[loc],
                  levels = c(1:3),
                  labels = c("Correct", "Warning", "Error")),
    error_code = x$error_code[loc],
    error_desc = x$error_desc[loc]
  )
  return(res)
}

"%nin%" <- function(chrvelements, chrvset) {
  !(chrvelements %in% chrvset)
}
