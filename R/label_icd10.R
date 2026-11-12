#' Retrieve ICD-10 Labels
#'
#' Returns Chinese labels/descriptions for ICD-10 codes.
#' Multiple descriptions for the same code can be collapsed
#' into a single character string.
#'
#' @param x Character vector of ICD-10 codes.
#' @param sep Character string used to collapse multiple
#'   descriptions. Default is `"; "`.
#' @param desc_n Integer. Maximum number of descriptions
#'   to return for each ICD-10 code. If `NULL` (default),
#'   all available descriptions are returned.
#' @param lang Language used to export labels.
#'
#' @return A character vector containing ICD-10 labels.
#'   Returns `NA` for codes without matched labels.
#'
#'
#' If an ICD-10 code has multiple labels, they are combined
#' using `sep`.
#'
#' @examples
#' label_icd10(c("C16", "C18"))
#'
#' label_icd10(
#'   c("C16", "C18"),
#'   sep = "; "
#' )
#'
#' label_icd10(
#'   c("C16", "C18"),
#'   desc_n = 1
#' )
#'
#' @export
#'
#'
label_icd10 <- function(x, sep = ", ", desc_n = NULL, lang = "cn") {
  add_crcheck_labels(
    x,
    sep = sep,
    desc_n = desc_n,
    lang = lang,
    label_dict = "icd10"
  )
}


#' Retrieve ICD-O-3 Topography labels
#'
#' Returns Chinese labels/descriptions for ICD-O-3 topography codes.
#' Multiple descriptions for the same code can be collapsed
#' into a single character string.
#'
#' @param x Character vector of ICD-10 codes.
#' @param sep Character string used to collapse multiple
#'   descriptions. Default is `"; "`.
#' @param desc_n Integer. Maximum number of descriptions
#'   to return for each Topography code. If `NULL` (default),
#'   all available descriptions are returned.
#' @param lang Language used to export labels.
#'
#' @return A character vector containing ICD-O-3 topography labels.
#'   Returns `NA` for codes without matched labels.
#'
#'
#' @export
#'
#' @examples
#' label_topo(c("C16", "C18"))
#' label_topo(c("C16", "C18"), sep = "; ")
#' label_topo(c("42.1", "C42.2"), desc_n = 1)
#'
label_topo <- function(x, sep = ", ", desc_n = NULL, lang = "cn") {
  add_crcheck_labels(
    x,
    sep = sep,
    desc_n = desc_n,
    lang = lang,
    label_dict = "topo"
  )
}


#' Retrieve ICD-O-3 Morphology labels
#'
#' Returns Chinese labels/descriptions for ICD-O-3 morphology codes.
#' Multiple descriptions for the same code can be collapsed
#' into a single character string.
#'
#' @param x Character vector of ICD-10 morphology codes.
#' @param sep Character string used to collapse multiple
#'   descriptions. Default is `"; "`.
#' @param desc_n Integer. Maximum number of descriptions
#'   to return for each Topography code. If `NULL` (default),
#'   all available descriptions are returned.
#' @param lang Language used to export labels.
#'
#' @return A character vector containing ICD-O-3 morphology labels.
#'   Returns `NA` for codes without matched labels.
#'
#'
#' @export
#'
#' @examples
#' label_topo(c("C16", "C18"))
#' label_topo(c("C16", "C18"), sep = "; ")
#' label_topo(c("42.1", "C42.2"), desc_n = 1)
#'
label_morp <- function(x, sep = ", ", desc_n = NULL, lang = "cn") {
  add_crcheck_labels(
    x,
    sep = sep,
    desc_n = desc_n,
    lang = lang,
    label_dict = "morp"
  )
}

#' Retrieve labels
#'
#' @param x Character vector of variable codes.
#' @param sep Character string used to collapse multiple
#'   descriptions. Default is `"; "`.
#' @param desc_n Integer. Maximum number of descriptions
#'   to return for each variable code. If `NULL` (default),
#'   all available descriptions are returned.
#' @param lang Language used to export labels.
#' @param label_dict Label dict to export labels.
#'
#' @noRd
#'
add_crcheck_labels <- function(
  x,
  sep = "; ",
  desc_n = NULL,
  lang = "cn",
  label_dict = c("icd10", "topo", "morp")
) {
  label_dict <- match.arg(label_dict)
  dict <- switch(
    label_dict,
    icd10 = Label_ICD10_cn,
    topo = Label_topo_cn,
    morp = Label_morp_cn
  )
  res <- lapply(dict[x], function(aa) {
    if (is.null(aa) || all(is.na(aa))) {
      return(NA_character_)
    }
    n <- length(aa)
    if (is.null(desc_n)) {
      use_n <- n
    } else {
      use_n <- min(desc_n, n)
    }
    paste(aa[seq_len(use_n)], collapse = sep)
  })
  unname(unlist(res))
}
