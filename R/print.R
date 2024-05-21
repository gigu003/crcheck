#' Title
#'
#' @param x check class
#' @param ... other
#'
#' @return print
#' @export
#'
print.check <- function(x, ...) {
  cat("Check Items:", x$check_item, "\n")
  cat("Check Method:", x$check_version, "\n")
  cat("Check Results:")
  print(table(x$type))
  invisible(x)
}

'%nin%' <- function(chrVElements, chrVSet){
  !(chrVElements %in% chrVSet)
}