#' @return A list object of class `check` with the following elements:
#'
#' - `check_item`: A character string indicating the item or combination of
#'    items being checked.
#' - `check_standard`: A character string specifying the standard used.
#' - `check_version`: A character string specifying the version of the standard.
#' - `data`: A list containing the input data.
#' - `check`: A logical vector indicating whether each code passed the checks.
#' - `type`: A numeric vector categorizing validation results: `1` for correct,
#'    `2` for Warning, `3` for Error.
#' - `error_code`: A character vector containing error or warning codes.
#' - `error_desc`: A character vector containing descriptive messages for each
#'    error or warning.
