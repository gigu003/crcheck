#' Add labels to check output messages
#'
#' Adds Chinese or English labels to check result data frames by matching the
#' `type` and `group` columns against the internal `dict_output_message`
#' dictionary.
#'
#' @param x A data frame containing check results. It must contain columns
#'   `type` and `group`. Objects of class `check` are converted with
#'   `print()` before labels are added.
#' @param lang A single character string specifying the label language. Use
#'   `"cn"` for Chinese labels or `"en"` for English labels.
#' @param label_col A single character string specifying the output label column
#'   name. If `NULL`, `"label_cn"` or `"label_en"` is used according to
#'   `lang`.
#'
#' @return A data frame with the original columns from `x` and one additional
#'   label column. Unmatched `type` and `group` combinations are labelled as
#'   `NA_character_`.
#'
#' @details
#' The matching key is the combination of `type` and `group`. Single-item
#' messages (`E-MISS`, `E-FORM`, and `E-RANGE`) fall back to the generic
#' `G-XX` message when an item-specific group is not found. The function uses
#' base R only and does not modify row order.
#'
#' @examples
#' x <- data.frame(
#'   id = "case-1",
#'   item = "sex",
#'   value = "9",
#'   type = "E-RANGE",
#'   group = "G-02",
#'   stringsAsFactors = FALSE
#' )
#'
#' label_message(x)
#' label_message(x, lang = "en")
#'
#' @export
label_message <- function(x, lang = c("cn", "en"), label_col = NULL) {
  lang <- match.arg(lang)

  if (inherits(x, "check") && !is.data.frame(x)) {
    x <- print(x)
  }

  check_data_frame(x)
  check_required_columns(x, c("type", "group"))

  if (is.null(label_col)) {
    label_col <- paste0("label_", lang)
  } else if (
    !is.character(label_col) ||
      length(label_col) != 1L ||
      is.na(label_col) ||
      !nzchar(label_col)
  ) {
    stop(
      "`label_col` must be a single non-empty character string.",
      call. = FALSE
    )
  }

  dict_label_col <- paste0("label_", lang)
  dict_key <- paste(
    dict_output_message$type,
    dict_output_message$group,
    sep = "/"
  )
  x_key <- paste(x$type, x$group, sep = "/")
  pos <- match(x_key, dict_key)

  idx_missing <- is.na(pos)
  if (any(idx_missing)) {
    generic_key <- paste(x$type[idx_missing], "G-XX", sep = "/")
    pos[idx_missing] <- match(generic_key, dict_key)
  }

  x[[label_col]] <- dict_output_message[[dict_label_col]][pos]
  x
}

#' Check that a data frame contains required columns
#'
#' @noRd
check_required_columns <- function(x, cols, arg = "x") {
  check_data_frame(x, arg)

  if (!is.character(cols)) {
    stop("`cols` must be a character vector.", call. = FALSE)
  }

  missing_cols <- cols[!cols %in% names(x)]
  if (length(missing_cols) > 0L) {
    stop(
      "The following columns are missing from `",
      arg,
      "`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
