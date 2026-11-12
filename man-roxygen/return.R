#' @return A list object of class `check` with the following elements:
#'
#' - `check_item`: A character vector indicating the item or item combination
#'   being checked.
#' - `check`: An integer vector giving the positions of records with warnings or
#'   errors.
#' - `data`: A character vector containing the checked values for records with
#'   warnings or errors.
#' - `type`: A character vector of warning or error codes.
#' - `group`: A character vector of rule group identifiers.
#'
#' Printing a `check` object returns a data frame with columns `id`, `item`,
#' `value`, `type`, and `group`.
