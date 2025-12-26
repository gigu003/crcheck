#' Recode the morphology code in a string
#'
#' @param x A string contains morphology code
#'
#' @return Vector
#' @export
#'
recode_morp <- function(x) {
  trans <- gsub(" ", "", x)
  sort(unlist(lapply(trans, fullmorp)))
}

fullmorp <- function(x){
  if (grepl("^[0-9]{4}-[0-9]{4}$", x)) {
    start <- as.integer(substr(x, 1, 4))
    end <- as.integer(substr(x, 6, 9))
    gen_morp(start:end)
  } else if (grepl("^[0-9]{4}$", x)) {
    gen_morp(as.integer(x))
  } else if (grepl("^M[0-9]{4}$", x)) {
    gen_morp(as.integer(gsub("[^0-9]", "", x)))
  } else {
    NA
  }
}


#' Recode the topography code in a string
#'
#' @param x A string contains topography code
#'
#' @return Vector
#' @export
#'
recode_topo <- function(x) {
  trans <- gsub(" ", "", x)
  sort(unlist(lapply(trans, fulltopo)))
}

fulltopo <- function(x) {
  if (grepl("^C\\d{2}-C\\d{2}$", x)) {
    start <- as.integer(substr(x, 2, 3)) * 10
    end <- as.integer(substr(x, 6, 7)) * 10 + 9
    gen_topo(start:end)
  } else if (grepl("^C\\d{2}$", x)) {
    start <- as.integer(substr(x, 2, 3)) * 10
    end <- start + 9
    gen_topo(start:end)
  } else if (grepl("^C\\d{3}$", x)) {
    start <- as.integer(gsub("[^0-9]", "", x))
    gen_topo(start)
  } else if (grepl("^\\d{3}$", x)) {
    start <- as.integer(gsub("[^0-9]", "", x))
    gen_topo(start)
  } else if (grepl("^C\\d{3}-C\\d{3}$", x)) {
    start <- as.integer(substr(x, 2, 4))
    end <- as.integer(substr(x, 7, 9))
    gen_topo(start:end)
  } else if (grepl("^\\d{3}-\\d{3}$", x)) {
    start <- as.integer(substr(x, 1, 3))
    end <- as.integer(substr(x, 5, 7))
    gen_topo(start:end)
  } else {
    NA
  }
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
#' @param min_range Integer. Minimum length of a run to compress. Default is 3.
#'
#' @return A character string with compressed ranges.
#'
#' @export
#' @examples
#' compress_morp(c(8000, 8001, 8002, 8003, 9594, 8006))
#' compress_morp(c(8000, 8001, 8002), min_range = 4)
compress_morp <- function(x,
                          sep = ",",
                          pattern = "%04d",
                          conn = "-",
                          min_range = 3) {
  if (!is.numeric(x)) stop("`x` must be a numeric or integer vector.")
  x <- sort(unique(as.integer(x)))
  result <- c()
  i <- 1
  n <- length(x)
  
  while (i <= n) {
    start <- x[i]
    end <- start
    while (i < n && x[i + 1] == x[i] + 1) {
      i <- i + 1
      end <- x[i]
    }
    len <- end - start + 1
    if (len >= min_range) {
      result <- c(result, sprintf(paste0(pattern, conn, pattern), start, end))
    } else {
      result <- c(result, sprintf(pattern, seq(start, end)))
    }
    i <- i + 1
  }
  
  paste(result, collapse = sep)
}

#' Compress ICD-O-3 topography codes
#'
#' If all subcodes from CXX.0 to CXX.9 are present, compress to "CXX".
#' Otherwise, list each code individually. Additionally, compress sequential
#' top-level codes (e.g., "C01,C02,C03" → "C01-C03") when possible.
#'
#' @param topo A character vector of ICD-O-3 topography codes (e.g., "C17.0").
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
compress_topo <- function(topo, sep = ",", conn = "-", min_range = 3) {
  if (length(topo) == 0) return("")
  topo <- sort(unique(topo))
  base <- sub("\\..*", "", topo)
  topo_groups <- split(topo, base)
  
  compressed <- lapply(names(topo_groups), function(b) {
    base_num <- as.integer(sub("C", "", b))
    expected <- gen_topo(base_num * 10 + 0:9)
    observed <- topo_groups[[b]]
    if (all(expected %in% observed)) {
      b
    } else {
      observed
    }
  })
  
  pattern <- "^C\\d{2}$"
  # Initialize
  temp <- character()
  result <- list()
  add_to_result <- function(val) {
    result[[length(result) + 1]] <<- val
  }
  
  # Main loop
  for (item in compressed) {
    if (length(item) == 1 && grepl(pattern, item)) {
      temp <- c(temp, item)
    } else {
      if (length(temp) > 0) {
        nums <- as.integer(gsub("C", "", temp))
        add_to_result(compress_morp(nums, pattern = "C%02d",
                                    conn = conn, sep = sep,
                                    min_range = min_range))
        temp <- character()
      }
      add_to_result(item)
    }
  }
  # Final tail
  if (length(temp) > 0) {
    nums <- as.integer(gsub("C", "", temp))
    add_to_result(compress_morp(nums, pattern = "C%02d",
                                conn = conn, sep = sep, min_range = min_range))
  }
  
  paste(unlist(result), collapse = sep)

}
