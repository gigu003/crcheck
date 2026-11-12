#' Check that an object is a data frame
#'
#' @noRd
check_data_frame <- function(x, arg = "x") {
  if (!is.data.frame(x)) {
    stop("`", arg, "` must be a data frame.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Check that an object is a single TRUE/FALSE value
#'
#' @noRd
check_flag <- function(x, arg = "x") {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Convert a data frame to a tibble when requested
#'
#' @noRd
maybe_as_tibble <- function(x, as_tibble = FALSE) {
  check_flag(as_tibble, "as_tibble")

  if (!as_tibble) {
    return(x)
  }

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop(
      "Package `tibble` is required when `as_tibble = TRUE`. ",
      "Please install it with install.packages(\"tibble\").",
      call. = FALSE
    )
  }

  tibble::as_tibble(x)
}

#' Check that vectors have the same length
#'
#' @noRd
check_equal_length <- function(..., arg_names = NULL) {
  xs <- list(...)
  lens <- vapply(xs, length, integer(1))

  if (length(unique(lens)) > 1L) {
    if (is.null(arg_names)) {
      arg_names <- paste0("argument ", seq_along(xs))
    }

    stop(
      "Arguments must have the same length: ",
      paste(paste0("`", arg_names, "` = ", lens), collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Parse common cancer registry date formats
#'
#' @noRd
parse_registry_date <- function(x, arg = "x") {
  if (inherits(x, "Date")) {
    return(x)
  }

  x_chr <- as.character(x)
  x_chr <- trimws(x_chr)
  x_chr[x_chr %in% c("", "NULL", "NA")] <- NA_character_

  res <- as.Date(rep(NA_character_, length(x_chr)))
  idx <- !is.na(x_chr)
  formats <- c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d")

  for (fmt in formats) {
    idx_missing <- idx & is.na(res)
    if (!any(idx_missing)) {
      break
    }

    parsed <- suppressWarnings(as.Date(x_chr[idx_missing], format = fmt))
    res[idx_missing] <- parsed
  }

  res
}

#' Get a data frame column by case-insensitive name
#'
#' @noRd
get_column_ci <- function(x, name) {
  pos <- match(tolower(name), tolower(names(x)))
  if (is.na(pos)) {
    return(NULL)
  }

  x[[pos]]
}
