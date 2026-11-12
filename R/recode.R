#' Recode the morphology code in a string
#'
#' @param x A string contains morphology code

#' @param sep A character string used to split multiple expressions
#'   within each element of `x`. Default is ",".
#' @param ignore Character vector of symbols to remove prior to parsing
#'   (e.g. c("M", "m")).
#' @param simplify Logical; if `TRUE` and `x` has length 1, return a
#'   simplified integer vector instead of a list. Default is `TRUE`.
#' @param sort_result Logical; if `TRUE`, results are sorted. If `FALSE`,
#'   unique values are returned while preserving input order.
#'
#' @return Vector
#' @export
#'
recode_morp <- function(
  x,
  sep = ",",
  ignore = "M",
  simplify = TRUE,
  sort_result = FALSE
) {
  x <- as.character(x)
  res <- lapply(x, function(.x) {
    parts <- unlist(strsplit(.x, sep, fixed = TRUE))
    parts <- trimws(parts)

    vals <- unlist(fullmorp(parts, ignore = ignore), use.names = FALSE)

    if (sort_result) {
      vals <- sort(vals, na.last = TRUE)
    } else {
      vals <- unique(vals)
    }

    vals
  })

  # 仅在单输入时简化
  if (simplify && length(res) == 1) {
    return(res[[1]])
  }

  res
}


#' Expand morphology code expressions
#'
#' Internal helper to parse and expand morphology codes.
#' Supports:
#' \itemize{
#'   \item Single codes (e.g. "8000")
#'   \item Ranges (e.g. "8000-8005" or "M8000-8005", or "M8000-M8005")
#'   \item Codes with prefix (e.g. "M8000")
#' }
#'
#' @param x Character vector of morphology code expressions.
#' @param ignore Character vector of symbols to remove before parsing.
#'   Default is "M", which removes morphology prefixes such as "M8000".
#' @return A list of integer vectors.
#' @keywords internal
#' @noRd
fullmorp <- function(x, ignore = "M") {
  res <- vector("list", length(x))

  # Remove characters before parsing
  if (!is.null(ignore)) {
    pattern <- paste0("[", paste(ignore, collapse = ""), "]")
    x_clean <- gsub(pattern, "", x)
  } else {
    x_clean <- x
  }

  # Condition 1, Range value
  idx1 <- grepl("^[0-9]{4}-[0-9]{4}$", x_clean)
  if (any(idx1)) {
    start <- as.integer(substr(x_clean[idx1], 1, 4))
    end <- as.integer(substr(x_clean[idx1], 6, 9))

    res[idx1] <- Map(function(s, e) gen_morp(s:e), start, end)
  }

  # Condition 2, Single value
  idx2 <- grepl("^[0-9]{4}$", x_clean)
  if (any(idx2)) {
    res[idx2] <- lapply(as.integer(x_clean[idx2]), gen_morp)
  }

  # Condition 3, Others
  idx_other <- !(idx1 | idx2)
  if (any(idx_other)) {
    res[idx_other] <- list(NA_integer_)
  }

  res
}

#' Recode the topography code in a string
#'
#' @param x A string contains topography code
#' @param sep A character string used to split multiple expressions
#'   within each element of `x`. Default is ",".
#' @param simplify Logical; if `TRUE` and `x` has length 1, return a
#'   simplified integer vector instead of a list. Default is `TRUE`.
#' @param sort_result Logical; if `TRUE`, results are sorted. If `FALSE`,
#'   unique values are returned while preserving input order.
#' @return Vector
#' @export
#'
recode_topo <- function(x, sep = ",", simplify = TRUE, sort_result = FALSE) {
  if (!is.character(x)) {
    stop("`x` must be a character vector")
  }

  res <- lapply(x, function(.x) {
    parts <- unlist(strsplit(.x, sep, fixed = TRUE))
    parts <- trimws(parts)

    vals <- unlist(fulltopo(parts), use.names = FALSE)

    if (sort_result) {
      vals <- sort(vals, na.last = TRUE)
    } else {
      vals <- unique(vals)
    }

    vals
  })

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }

  res
}

#' Expand topography code expressions
#'
#' Internal helper to parse and expand topography codes.
#' Supports:
#' \itemize{
#'   \item Single codes (e.g. "C159", "C20")
#'   \item Ranges (e.g. "C159-C169", "C33-C34")
#' }
#'
#' @param x Character vector of morphology code expressions.
#' @return A list of character vectors containing topography codes.
#' @keywords internal
#' @noRd
fulltopo <- function(x) {
  x <- toupper(x)
  x <- gsub("\\.", "", x)
  res <- vector("list", length(x))

  # Cxx-Cxx
  idx1 <- grepl("^C\\d{2}-C\\d{2}$", x)
  if (any(idx1)) {
    start <- as.integer(substr(x[idx1], 2, 3)) * 10
    end <- as.integer(substr(x[idx1], 6, 7)) * 10 + 9
    res[idx1] <- Map(function(s, e) gen_topo(s:e), start, end)
  }

  # Cxx
  idx2 <- grepl("^C\\d{2}$", x)
  if (any(idx2)) {
    start <- as.integer(substr(x[idx2], 2, 3)) * 10
    end <- start + 9
    res[idx2] <- Map(function(s, e) gen_topo(s:e), start, end)
  }

  # Cxxx
  idx3 <- grepl("^C\\d{3}$", x)
  if (any(idx3)) {
    start <- as.integer(sub("^C", "", x[idx3]))
    res[idx3] <- lapply(start, gen_topo)
  }

  # xxx
  idx4 <- grepl("^\\d{3}$", x)
  if (any(idx4)) {
    start <- as.integer(x[idx4])
    res[idx4] <- lapply(start, gen_topo)
  }

  # Cxxx-Cxxx
  idx5 <- grepl("^C\\d{3}-C\\d{3}$", x)
  if (any(idx5)) {
    start <- as.integer(substr(x[idx5], 2, 4))
    end <- as.integer(substr(x[idx5], 7, 9))
    res[idx5] <- Map(function(s, e) gen_topo(s:e), start, end)
  }

  # xxx-xxx
  idx6 <- grepl("^\\d{3}-\\d{3}$", x)
  if (any(idx6)) {
    start <- as.integer(substr(x[idx6], 1, 3))
    end <- as.integer(substr(x[idx6], 5, 7))
    res[idx6] <- Map(function(s, e) gen_topo(s:e), start, end)
  }

  # 其他情况
  idx_other <- !(idx1 | idx2 | idx3 | idx4 | idx5 | idx6)
  if (any(idx_other)) {
    res[idx_other] <- list(character(0))
  }

  res
}

#' Compress a morphology code vector into a compact range string
#'
#' Converts a numeric vector (e.g., ICD-O-3 morphology codes) into a compact
#' character string using hyphens for consecutive values and commas for
#' separation.
#'
#' @param x A numeric or integer vector of morphology codes.
#' @param sep A string used to separate values in the output. Default is ",".
#' @param pattern A format string passed to `sprintf()` (e.g., "%04d").
#' @param conn A character used to connect range start and end. Default is "-".
#' @param min_range Integer, Minimum length of a run to compress. Default is 3.
#' @param warn Logical, whether output the warnring message.
#'
#' @return A character string with compressed ranges.
#'
#' @export
#' @examples
#' compress_morp(c(8000, 8001, 8002, 8003))
#' compress_morp(c(8000, 8001, 8002), min_range = 4)
compress_morp <- function(
  x,
  sep = ",",
  pattern = "%04d",
  conn = "-",
  min_range = 3,
  warn = TRUE
) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric or integer.")
  }

  x <- sort(unique(as.integer(x)))
  x <- x[!is.na(x)]

  pos <- match(x, gen_morp(8000:9993))

  # 🔥 剔除非法值
  if (anyNA(pos)) {
    invalid_vals <- x[is.na(pos)]
    if (warn) {
      warning(
        sprintf(
          "Removed invalid morphology codes: %s",
          paste(invalid_vals, collapse = ", ")
        )
      )
    }
    x <- x[!is.na(pos)]
    pos <- pos[!is.na(pos)]
  }

  if (length(x) == 0) {
    return("")
  }

  res <- vector("list", 0)

  i <- 1
  n <- length(pos)

  while (i <= n) {
    start_i <- i

    while (i < n && pos[i + 1] == pos[i] + 1) {
      i <- i + 1
    }

    end_i <- i
    len <- end_i - start_i + 1

    start_val <- x[start_i]
    end_val <- x[end_i]

    if (len >= min_range) {
      res[[length(res) + 1]] <- sprintf(
        paste0(pattern, conn, pattern),
        start_val,
        end_val
      )
    } else {
      res[[length(res) + 1]] <- sprintf(pattern, x[start_i:end_i])
    }

    i <- i + 1
  }

  paste(unlist(res), collapse = sep)
}

#' Compress ICD-O-3 topography codes
#'
#' If all subcodes from CXX.0 to CXX.9 are present, compress to "CXX".
#' Otherwise, list each code individually. Additionally, compress sequential
#' top-level codes (e.g., "C01,C02,C03" → "C01-C03") when possible.
#'
#' @param x A character vector of ICD-O-3 topography codes (e.g., "C17.0").
#' @param sep A string used to separate values in the output. Default is (",").
#' @param conn A string used to connect compressed ranges. Default is "-".
#' @param min_range Integer. Minimum number of sequential codes to compress into a range.
#'
#' @return A character string of compressed topography codes.
#' @export
#'
#' @examples
#' compress_topo(c("C17.0", "C17.1", "C17.2", "C17.3", "C17.4",
#'                 "C17.5", "C17.6", "C17.7", "C17.8", "C17.9"))
#'
#' compress_topo(c("C17.0", "C17.2", "C17.9"))
#'
#' compress_topo(c("C01.0", "C02.1", "C03.9", "C04.2", "C04.9"))
#'
#' compress_topo(gen_topo(10:19))
compress_topo <- function(x, sep = ",", conn = "-", min_range = 3) {
  compress_topo_icd10(
    x,
    dict = "topo",
    sep = sep,
    conn = conn,
    min_range = min_range
  )
}


#' Compress ICD-10 codes
#'
#' If all subcodes from CXX.0 to CXX.9 are present, compress to "CXX".
#' Otherwise, list each code individually. Additionally, compress sequential
#' top-level codes (e.g., "C01,C02,C03" → "C01-C03") when possible.
#'
#' @param x A character vector of ICD-10 codes (e.g., "C17.0").
#' @param sep A string used to separate values in the output. Default is (",").
#' @param conn A string used to connect compressed ranges. Default is "-".
#' @param min_range Integer. Minimum number of sequential codes to compress into a range.
#'
#' @return A character string of compressed ICD-10 codes.
#' @export
#'
#' @examples
#' compress_icd10(c("C17.0", "C17.2", "C17.9"))
compress_icd10 <- function(x, sep = ",", conn = "-", min_range = 3) {
  compress_topo_icd10(
    x,
    dict = "icd10",
    sep = sep,
    conn = conn,
    min_range = min_range
  )
}

compress_topo_icd10 <- function(
  x,
  dict = c("topo", "icd10"),
  sep = ",",
  conn = "-",
  min_range = 3
) {
  dict <- match.arg(dict)
  dict_codes <- if (dict == "topo") {
    dict_topo
  } else {
    dict_icd10
  }

  x <- sort(unique(as.character(x)))
  x <- x[!is.na(x)]

  x <- extract_maincodes(x, dict_codes)

  maincode <- x[grepl("^C\\d{2}$", x)]
  others <- x[!grepl("^C\\d{2}$", x)]

  valid <- names(dict_codes)
  pos <- match(maincode, valid)

  if (any(is.na(pos))) {
    stop("Some values are not valid topography codes.")
  }

  res <- vector("list", 0)

  i <- 1
  n <- length(pos)

  while (i <= n) {
    start_i <- i
    while (i < n && pos[i + 1] == pos[i] + 1) {
      i <- i + 1
    }

    end_i <- i
    len <- end_i - start_i + 1
    start_val <- maincode[start_i]
    end_val <- maincode[end_i]
    if (len >= min_range) {
      # 🔥 range compression
      res[[length(res) + 1]] <- paste0(start_val, conn, end_val)
    } else {
      res[[length(res) + 1]] <- paste(maincode[start_i:end_i], collapse = sep)
    }
    i <- i + 1
  }
  res <- sort(c(unlist(res), others))
  paste(res, collapse = sep)
}


extract_maincodes <- function(x, dict = dict_topo) {
  x <- unique(as.character(x))
  x <- x[!is.na(x)]
  res <- x
  dict_order <- names(dict)[
    order(lengths(dict), decreasing = TRUE)
  ]
  for (b in dict_order) {
    group <- dict[[b]]
    if (all(group %in% res)) {
      res <- setdiff(res, group)
      res <- c(res, b)
    }
  }
  sort(res)
}

#' Compress consecutive numeric values into interval strings
#'
#' Compress a numeric vector into a compact character representation by
#' collapsing consecutive values into ranges. Optionally supports left and
#' right censoring using specified minimum and maximum bounds.
#'
#' @param x A numeric or integer vector to be compressed.
#' @param sep A character string used to separate output segments.
#'   Default is ",".
#' @param conn A character string used to connect range boundaries.
#'   Default is "-".
#' @param min_val Optional numeric scalar specifying the minimum bound.
#'   If a range starts at \code{min_val}, it will be represented as
#'   \code{"<end+1"}.
#' @param max_val Optional numeric scalar specifying the maximum bound.
#'   If a range ends at \code{max_val}, it will be represented as
#'   \code{">start-1"}.
#'
#' @details
#' The function first sorts and deduplicates the input vector, then identifies
#' consecutive sequences.
#'
#' The following transformation rules are applied to each sequence:
#' \itemize{
#'   \item If \code{start == end}, return a single value.
#'   \item If \code{start != end}, return a range using \code{conn}.
#'   \item If \code{start == min_val}, apply left censoring:
#'         \code{"<end+1"}.
#'   \item If \code{end == max_val}, apply right censoring:
#'         \code{">start-1"}.
#' }
#'
#' Missing values (\code{NA}) are removed prior to processing.
#'
#' @return
#' A character string representing the compressed intervals.
#'
#' @examples
#' # Basic compression
#' compress_range(c(1:9, 22))
#' # "1-9,22"
#'
#' @export
compress_range <- function(
  x,
  sep = ",",
  conn = "-",
  min_val = 0,
  max_val = 120
) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }

  x <- sort(unique(as.integer(x)))
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return("")
  }
  res <- character()
  start <- x[1]
  prev <- x[1]

  for (i in 2:length(x)) {
    if (x[i] == prev + 1) {
      prev <- x[i]
    } else {
      res <- c(res, format_segment(start, prev, conn, min_val, max_val))
      start <- x[i]
      prev <- x[i]
    }
  }

  # 最后一段
  res <- c(res, format_segment(start, prev, conn, min_val, max_val))
  paste(res, collapse = sep)
}

format_segment <- function(start, end, conn, min_val, max_val) {
  # Left censoring
  if (!is.null(min_val) && start == min_val) {
    return(paste0("<", end + 1))
  }

  # Right censoring
  if (!is.null(max_val) && end == max_val) {
    return(paste0(">", start - 1))
  }

  # Regular range
  if (start == end) {
    return(as.character(start))
  } else {
    return(paste0(start, conn, end))
  }
}
