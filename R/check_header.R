#' Check header variables according to schema
#'
#' This function validates whether the variable names of a data frame
#' conform to a predefined schema. It checks for:
#' \itemize{
#'   \item Missing mandatory variables
#'   \item Missing suggested variables
#'   \item Non-required (unexpected) variables
#' }
#'
#' @param x A data frame containing the dataset to be checked.
#' @param schema A character string specifying the schema name.
#'   Default is \code{"ncc"}. The schema must exist in \code{SchemaHeader}.
#'
#' @returns
#' An object of class \code{c("check", "Header")} containing:
#' \describe{
#'   \item{check_item}{Description of the check performed.}
#'   \item{check_standard}{Schema name used for validation.}
#'   \item{data}{Character vector describing problematic variables.}
#'   \item{check}{Numeric vector indicating check results (e.g., 0 = issue found).}
#'   \item{error_code}{Character vector of error or warning codes.}
#' }
#'
#' If no issues are found, the returned object will contain zero-length vectors,
#' which will produce a zero-row result when printed.
#'
#' @export
#'
#' @examples
#' # Example schema (simplified)
#' SchemaHeader <- list(
#'   ncc = list(
#'     man = c("id", "age", "sex"),
#'     suggest = c("address")
#'   )
#' )
#'
#' # Example data
#' df <- data.frame(
#'   id = 1:3,
#'   age = c(30, 40, 50),
#'   extra_var = 1
#' )
#'
#' # Run check
#' res <- check_header(df, schema = "ncc")
#' res
check_header <- function(x, schema = "ncc") {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame.", call. = FALSE)
  }

  if (!schema %in% names(SchemaHeader)) {
    stop(
      "`schema` must be one of: ",
      paste(names(SchemaHeader), collapse = ", "),
      call. = FALSE
    )
  }

  schema_vars <- SchemaHeader[[schema]]
  vars_present <- tolower(names(x))

  man_vars <- schema_vars[["man"]]
  suggest_vars <- schema_vars[["suggest"]]

  miss_man_vars <- man_vars[man_vars %nin% vars_present]
  miss_suggest_vars <- suggest_vars[suggest_vars %nin% vars_present]
  not_ness_vars <- vars_present[
    vars_present %nin% unlist(schema_vars, use.names = FALSE)
  ]

  # 构建结果（只保留非空项）
  values <- c(
    if (length(miss_man_vars) > 0) paste(miss_man_vars, collapse = ", "),
    if (length(miss_suggest_vars) > 0) {
      paste(miss_suggest_vars, collapse = ", ")
    },
    if (length(not_ness_vars) > 0) paste(not_ness_vars, collapse = ", ")
  )

  check <- rep(0, length(values))

  groups <- c(
    if (length(miss_man_vars) > 0) "01",
    if (length(miss_suggest_vars) > 0) "02",
    if (length(not_ness_vars) > 0) "03"
  )

  res <- structure(
    list(
      check_item = paste(toupper(schema), "Schema Header"),
      check_standard = schema,
      data = values,
      check = check,
      type = rep("W-ExVars", length(check)),
      group = paste0("G-", groups)
    ),
    class = c("check", "Header")
  )

  return(res)
}
