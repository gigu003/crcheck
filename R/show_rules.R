#' Show rules used in crcheck
#'
#' @param type Character, type of error or warning. Use `"mult_topo"` and
#'   `"mult_morp"` to show IARC/IACR 2004 multiple primary topography and
#'   morphology groups.
#' @param sep A string used to separate values in the output. Default is ",".
#'
#' @return A data frame containing rule groups.
#' @export
#'
show_rules <- function(type = "seto", sep = ", ") {
  if (length(type) != 1L || is.na(type)) {
    stop("Argument 'type' must be a single non-missing string.", call. = FALSE)
  }
  show_funs <- list(
    seto = show_seto,
    semo = show_semo,
    beto = show_beto,
    agmt = show_agmt,
    tomo = show_tomo,
    mogr = show_mogr,
    mobe = show_mobe,
    bdmo = show_bdmo,
    foup = show_foup,
    mult_topo = show_mult_topo,
    mult_morp = show_mult_morp
  )
  fun <- show_funs[[type]]
  if (is.null(fun)) {
    stop(sprintf("Unknown type: '%s'", type), call. = FALSE)
  }

  fun(sep = sep)
}

#' Internal: Generate rule group labels
#'
#' Create standardized group identifiers (e.g., "G-01", "G-02", ...).
#'
#' @param n Integer. Number of groups.
#' @return A character vector of length \code{n} with group labels.
#'
#' @keywords internal
#' @noRd
make_group <- function(n) {
  sprintf("G-%02d", seq_len(n))
}

#' Internal: Show sex-topography rules-SETO
#'
#' Generates a data frame of sex-topography rule groups used internally
#' in consistency checks.
#'
#' @param sep A string used to separate values in the output. Default is ", ".
#'
#' @return A data frame with columns group, sex, and topo.
#' @keywords internal
#' @noRd
show_seto <- function(sep = ", ") {
  data.frame(
    group = make_group(2),
    sex = c(1L, 2L),
    topo = c(
      compress_topo(gen_topo(510:589), sep = sep),
      compress_topo(gen_topo(600:639), sep = sep)
    ),
    stringsAsFactors = FALSE
  )
}

#' Internal: Show sex-morphology rules-SEMO
#'
#' Generates a data frame of sex-morphology rule groups used internally
#' in consistency checks.
#'
#' @param sep A string used to separate values in the output. Default is ", ".
#'
#' @return A data frame with columns group, sex, and morp.
#' @keywords internal
#' @noRd
show_semo <- function(sep = ", ") {
  data.frame(
    group = make_group(2),
    sex = c(1L, 2L),
    morp = c(
      compress_morp(do.call(c, IARC_TopoMorpRule[[1]][23:27]), sep = sep),
      compress_morp(do.call(c, IARC_TopoMorpRule[[1]][28:29]), sep = sep)
    ),
    stringsAsFactors = FALSE
  )
}


#' Internal: Show behaviour-topography rules-BETO
#'
#' Generates a data frame of behaviour-topography rule groups used internally
#' in consistency checks.
#'
#' @param sep A string used to separate values in the output. Default is ", ".
#'
#' @return A data frame with columns group, beha, and topo.
#' @keywords internal
#' @noRd
show_beto <- function(sep = ", ") {
  data.frame(
    group = make_group(1),
    beha = c(2L),
    topo = compress_topo(
      gen_topo(c(400:429, 470:479, 490:499, 700:729)),
      sep = sep
    ),
    stringsAsFactors = FALSE
  )
}

#' Internal: Show age-topography-morphology rules-AGMT
#'
#' Generates a data frame of age-topography-morphology rule groups used internally
#' in consistency checks.
#'
#' @param sep A string used to separate values in the output. Default is ", ".
#'
#' @return A data frame with columns age, topo, and morp.
#' @keywords internal
#' @noRd
show_agmt <- function(sep = ", ") {
  age <- vapply(
    IARC_AgeTopoMorpRule[[1]],
    compress_range,
    FUN.VALUE = character(1),
    max_val = 120,
    min_val = 0
  )

  topo <- vapply(
    IARC_AgeTopoMorpRule[[2]],
    compress_topo,
    FUN.VALUE = character(1),
    sep = sep
  )

  morp <- vapply(
    IARC_AgeTopoMorpRule[[3]],
    compress_morp,
    FUN.VALUE = character(1),
    sep = sep
  )

  data.frame(
    group = make_group(length(age)),
    age = age,
    topo = topo,
    morp = morp,
    stringsAsFactors = FALSE
  )
}

#' Internal: Show topography-morphology rules-TOMO
#'
#' Generates a data frame of topography-morphology rule groups used internally
#' in consistency checks.
#'
#' @param sep A string used to separate values in the output. Default is ", ".
#'
#' @return A data frame with columns group, morp, and topo.
#' @keywords internal
#' @noRd
show_tomo <- function(sep = ", ") {
  n <- length(IARC_TopoMorpRule[["morp"]])
  data.frame(
    group = make_group(n),
    morp = vapply(
      IARC_TopoMorpRule[["morp"]],
      compress_morp,
      FUN.VALUE = character(1),
      sep = sep
    ),
    topo = vapply(
      IARC_TopoMorpRule[["topo"]],
      compress_topo,
      FUN.VALUE = character(1),
      sep = sep
    ),
    stringsAsFactors = FALSE
  )
}


#' Internal: Show basi-morphology rules-BDMO
#'
#' Generates a data frame of basi-morphology rule groups used internally
#' in consistency checks.
#'
#' @param sep A string used to separate values in the output. Default is ", ".
#'
#' @return A data frame with columns group, basi, morp, and topo.
#' @keywords internal
#' @noRd
show_bdmo <- function(sep = ", ") {
  data.frame(
    group = make_group(2),
    basi = vapply(
      IARC_BasiMorpRule[["basi"]],
      paste,
      FUN.VALUE = character(1),
      collapse = sep
    ),
    morp = vapply(
      IARC_BasiMorpRule[["morp"]],
      compress_morp,
      FUN.VALUE = character(1),
      sep = sep
    ),
    topo = vapply(
      IARC_BasiMorpRule[["topo"]],
      compress_topo,
      FUN.VALUE = character(1),
      sep = sep
    ),
    stringsAsFactors = FALSE
  )
}

#' Internal: Show morphology-grad rules-MOGR
#'
#' Generates a data frame of morphology-grade rule groups used internally
#' in consistency checks.
#'
#' @param sep A string used to separate values in the output. Default is ", ".
#'
#' @return A data frame with columns group, grad, and morp.
#' @keywords internal
#' @noRd
show_mogr <- function(sep = ", ") {
  data.frame(
    group = make_group(11),
    grad = vapply(
      IARC_GradMorpRule[[1]],
      compress_range,
      FUN.VALUE = character(1),
      sep = sep
    ),
    morp = vapply(
      IARC_GradMorpRule[[2]],
      compress_morp,
      FUN.VALUE = character(1),
      sep = sep
    ),
    stringsAsFactors = FALSE
  )
}

#' Internal: Show morphology-behaviour rules-MOBE
#'
#' Generates a data frame of behaviour–morphology rule groups used internally
#' in consistency checks.
#'
#' @param sep A string used to separate values in the output. Default is ", ".
#'
#' @return A data frame with columns group, beha, and morp.
#' @keywords internal
#' @noRd
show_mobe <- function(sep = ", ") {
  data.frame(
    group = make_group(6),
    beha = names(IARC_BehaMorpRule),
    morp = vapply(
      IARC_BehaMorpRule,
      compress_morp,
      FUN.VALUE = character(1),
      sep = sep
    ),
    stringsAsFactors = FALSE
  )
}

#' Internal: Show follow up rules-FOUP
#'
#' Generates a data frame of follow up rule groups used internally
#' in consistency checks.
#'
#' @param sep A string used to separate values in the output. Default is ", ".
#'
#' @return A data frame with columns group, rule.
#' @keywords internal
#' @noRd
show_foup <- function(sep = ", ") {
  IARC_FollowUpRule
}

#' Internal: Show multiple primary topography rules
#'
#' @param sep A string used to separate values in the output.
#'
#' @return A data frame containing Table 1 topography groups.
#' @keywords internal
#' @noRd
show_mult_topo <- function(sep = ", ") {
  res <- IARC_MultiplePrimaryRule$topo_summary
  res$topo <- vapply(
    IARC_MultiplePrimaryRule$topo,
    compress_topo,
    FUN.VALUE = character(1),
    sep = sep
  )
  res[, c("group", "site", "topo", "label", "simultaneous_code")]
}

#' Internal: Show multiple primary morphology rules
#'
#' @param sep A string used to separate values in the output.
#'
#' @return A data frame containing Table 2 morphology groups.
#' @keywords internal
#' @noRd
show_mult_morp <- function(sep = ", ") {
  res <- IARC_MultiplePrimaryRule$morp_summary
  res$morp <- vapply(
    IARC_MultiplePrimaryRule$morp,
    compress_morp,
    FUN.VALUE = character(1),
    sep = sep,
    warn = FALSE
  )
  res
}
