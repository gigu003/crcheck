#' Show the summary of the check result
#'
#' @param object check class
#' @param ... Other parameters
#'
#' @return summary
#' @export
#'
summary.check <- function(object, ...) {
  cat("Check Results:\n")
  errors <- length(object$group)
  cat(errors, " Error or Warnings were found.\n")
  if (errors != 0) {
    res <- as.data.frame(table(object$group))
    names(res) <- c("Group", "Count")
    print(res)
  }
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
  if (length(x$check) == 0) {
    return(data.frame(
      id = character(0),
      item = character(0),
      value = character(0),
      type = character(0),
      group = character(0)
    ))
  }

  check_item <- x$check_item
  if (length(check_item) == 1) {
    check_item <- rep(check_item, length(x$check))
  }
  type <- x$type
  if (length(type) == 1) {
    type <- rep(type, length(x$check))
  }

  data.frame(
    id = x$check,
    item = check_item,
    value = x$data,
    type = type,
    group = x$group
  )
}
