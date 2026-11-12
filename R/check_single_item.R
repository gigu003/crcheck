#' Check single item of the cancer registry data
#'
#' @param x Values to be checked.
#' @param var_name Name of the variable checked.
#' @param allow_missing Logical, indicates if allow missing value.
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' topo <- c("C51.0", "C53.0", "C57.4", "C60.0", "C60.9", "C63.9")
#' res <- check_single_item(topo, var_name = "topo")
#' print(res)
check_single_item <- function(
  x,
  var_name = "sex",
  allow_missing = NULL,
  quiet = FALSE
) {
  if (!is.character(var_name) || length(var_name) != 1L || is.na(var_name)) {
    stop("`var_name` must be a single character string.", call. = FALSE)
  }

  valid_var_names <- unique(c(names(ValueRange), "icd10"))
  if (!var_name %in% valid_var_names) {
    stop(
      "`var_name` must be one of: ",
      paste(valid_var_names, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(allow_missing)) {
    check_flag(allow_missing, "allow_missing")
  }
  check_flag(quiet, "quiet")

  if (var_name == "topo") {
    x <- add_decimal(x)
  }
  if (var_name == "icd10" & is.null(allow_missing)) {
    allow_missing <- FALSE
  }
  is_miss <- is.na(x) | x %in% c("NULL", "")
  value_std <- ValueRange[[var_name]]
  if (is.null(allow_missing)) {
    allow_missing <- value_std[["allow_missing"]]
  }
  loc_miss <- which(is_miss)

  if (var_name == "icd10") {
    loc_form <- which(invalid_icd10_format(x) & !is_miss)
  } else {
    loc_form <- which(!is_miss & !grepl(value_std$form, x))
  }

  if (var_name == "topo") {
    topo_range_check <- function(x) {
      topo_number <- gsub("[^0-9]", "", x)
      full <- sprintf("C%03s", topo_number)
      which(full %nin% value_std$range & !is_miss)
    }
    loc_range <- topo_range_check(x)
  } else if (var_name == "icd10") {
    loc_range <- which(invalid_icd10_range(x) & !is_miss)
  } else if (var_name == "id") {
    loc_range <- integer(0L)
  } else {
    loc_range <- which(x %nin% value_std$range & !is_miss)
  }

  # ---- 组合错误位置 ----
  if (allow_missing) {
    loc_miss_form_range <- list(integer(0), loc_form, loc_range)
  } else {
    loc_miss_form_range <- list(loc_miss, loc_form, loc_range)
  }

  check <- unique(unlist(loc_miss_form_range))
  error_code <- character(length(check))
  for (i in 3:1) {
    idx <- match(loc_miss_form_range[[i]], check)
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) {
      next
    }
    #if (allow_miss == TRUE & i == 1) next
    code <- ifelse(i == 1, "MISS", ifelse(i == 2, "FORM", "RANGE"))
    error_code[idx] <- paste0("E-", code)
  }

  ncc_vars <- c(
    "id",
    "sex",
    "age",
    "birthda",
    "inciden",
    "topo",
    "morp",
    "beha",
    "grad",
    "basi",
    "icd10",
    "deathda",
    "mult",
    "trib",
    "marri",
    "lastcontact",
    "status",
    "caus"
  )
  group <- match(var_name, ncc_vars)
  group <- paste0("G-", sprintf("%02d", group))

  # Construct result
  res <- structure(
    list(
      check_item = var_name,
      check = check,
      data = x[check],
      type = error_code,
      group = rep(group, length(check))
    ),
    class = c("check", var_name)
  )

  # Add extra check for ids
  if (var_name == "id") {
    loc_id_dates <- which(
      !is.na(x) & is.na(as.Date(substr(x, 7, 14), format = "%Y%m%d"))
    )
    loc_id_last_char <- id_last_unvalid_idx(x)
    check_id <- unique(c(loc_id_dates, loc_id_last_char))
    error_code_id <- character(length(check_id))
    error_code_id[match(loc_id_last_char, check_id)] <- "E-ID-LastChar"
    error_code_id[match(loc_id_dates, check_id)] <- "E-ID-Date"

    res$data <- c(res$data, x[check_id])
    res$check <- c(res$check, check_id)
    res$type <- c(res$type, error_code_id)
    res$group <- c(res$group, rep("G-01", length(check_id)))
  }

  if (!quiet) {
    summary(res)
  }
  invisible(res)
}

#' ICD10 format check
#'
#' @noRd
#' @param x ICD10 number.
#' @returns Logical vector with the same length of x.
invalid_icd10_format <- function(icd10) {
  special_c <- c(
    1,
    7,
    12,
    19,
    20,
    23,
    33,
    37,
    52,
    55,
    56,
    58,
    61,
    64,
    65,
    66,
    73,
    80,
    97
  )
  special_d <- c(24, 27, 34, 45)
  special_icd10 <- c(sprintf("C%02d", special_c), paste0("D", special_d))
  special2 <- icd10 %nin% paste0(special_icd10, ".0")
  regular <- grepl("^[CD]\\d{2}\\.\\d$", icd10)
  check <- ifelse(
    icd10 %in% special_icd10,
    TRUE,
    ifelse(regular & special2, TRUE, FALSE)
  )
  !check
}

#'  ICD10 range check
#'
#' @noRd
#' @param x ICD10 number.
#' @returns Logical vector with the same length of x.
invalid_icd10_range <- function(x) {
  x <- as.character(x)
  init <- toupper(substr(x, 1, 1))
  icdn <- suppressWarnings(
    as.numeric(gsub("[^0-9\\.]", "", x))
  )
  ress <- rep(NA_character_, length(x))
  # ---- D codes ----
  idx_d <- init == "D" & !is.na(icdn)
  ress[idx_d] <- gen_icd10(icdn[idx_d] * 10, init = "D", na_drop = FALSE)
  # ---- C codes ----
  idx_c <- init == "C" & !is.na(icdn)
  ress[idx_c] <- gen_icd10(icdn[idx_c] * 10, na_drop = FALSE)
  ## ---- Other codes will missing
  ress != x & !is.na(ress)
}

#' Convert the ICDO3 topo code to the complete format
#'
#' @noRd
#' @param x Topo codes.
#'
#' @returns Complete format of topo codes.
#'
add_decimal <- function(x) {
  #x <- gsub("[[:space:]　]+", "", x)
  ifelse(grepl("^C[0-9]{3}$", x), sub("([0-9]{2})([0-9])$", "\\1.\\2", x), x)
}

#' Validation of the last char of the id number
#'
#' @noRd
#' @param x Id number.
#' @returns Integer vector of invalid positions.
id_last_unvalid_idx <- function(x) {
  n <- length(x)
  res <- rep(NA, n)

  # ---- 基础校验 ----
  date_str <- substr(x, 7, 14)
  valid_date <- !is.na(suppressWarnings(as.Date(date_str, "%Y%m%d")))

  valid_idx <- !is.na(x) &
    nchar(x) == 18 &
    x != "NA" &
    valid_date

  if (!any(valid_idx)) {
    return(integer(0L))
  }

  x_valid <- x[valid_idx]

  # ---- 前17位必须是数字 ----
  is_digit <- grepl("^[0-9]{17}$", substr(x_valid, 1, 17))

  idx2 <- which(valid_idx)[is_digit]
  x_valid <- x[idx2]

  if (length(x_valid) == 0) {
    return(integer(0L))
  }

  # ---- 构造矩阵（关键修复）----
  chars <- do.call(rbind, strsplit(substr(x_valid, 1, 17), ""))
  #mat <- matrix(as.numeric(chars), ncol = 17)
  #mat <- apply(chars, 2, as.numeric)
  storage.mode(chars) <- "numeric"

  # ---- 权重 ----
  wi <- c(7, 9, 10, 5, 8, 4, 2, 1, 6, 3, 7, 9, 10, 5, 8, 4, 2)

  # ---- 计算 ----
  s <- chars %*% wi
  re <- s %% 11

  # ---- 映射 ----
  a1_map <- c("1", "0", "X", "9", "8", "7", "6", "5", "4", "3", "2")
  expect_last_char <- a1_map[re + 1]
  last_char <- substr(x_valid, 18, 18)
  res[idx2] <- last_char != expect_last_char
  which(!is.na(res) & res)
}
