#' Guess morphology codes from text descriptions
#'
#' Match Chinese or English morphology description text to the closest
#' ICD-O-3 morphology codes using approximate string matching.
#'
#' @param x Character vector of morphology description text (Chinese or English).
#' @param lang Language of the input text. `"cn"` for Chinese, `"en"` for English.
#' @param max.distance Maximum distance allowed for `agrep` fuzzy matching.
#'   Passed to [agrep()]. Default is `0.1`.
#' @param ... Additional arguments passed to [agrep()].
#'
#' @return A character vector of matched morphology codes. Multiple matches
#'   for a single input are collapsed with `", "`. Returns `NA_character_`
#'   when no match is found.
#'
#' @examples
#' guess_morp("adenocarcinoma")
#' guess_morp("癌")
#' guess_morp(c("adenocarcinoma", "lymphoma"))
#'
#' @export
guess_morp <- function(x, lang = c("cn", "en"), max.distance = 0.1, ...) {
  lang <- match.arg(lang)

  col <- if (lang == "cn") "morp_descn" else "morp_desen"

  descs <- dict_morp[[col]]
  codes <- dict_morp[["morp"]]
  agrep_args <- list(...)
  vapply(
    x,
    function(pattern) {
      args <- c(
        list(pattern = pattern, x = descs, max.distance = max.distance),
        agrep_args
      )
      idx <- do.call(agrep, args)
      if (length(idx) == 0) {
        return(NA_character_)
      }
      paste(unique(codes[idx]), collapse = ", ")
    },
    character(1)
  )
}
