#' Conduct quality checks for cancer registry data
#'
#' @rdname cr_check
#' @param x An object containing cancer registry data. Methods are available for
#'   data frames and CanReg-style objects.
#' @param schema A single character string specifying the schema name used for
#'   optional header checks. The default is `"ncc"`. The schema must exist in the
#'   internal `SchemaHeader` object.
#' @param header_check Logical. A single `TRUE` or `FALSE` value indicating
#'   whether to check variable names against the selected schema.
#' @param collapse Logical. Only used when `x` is a `canregs` object. If `TRUE`,
#'   results from multiple registries are combined into a single data frame. If
#'   `FALSE`, a list of results is returned.
#' @param as_tibble Logical. If `TRUE`, return a tibble. This requires the
#'   suggested package `tibble` to be installed. The default `FALSE` returns a
#'   base R data frame for backward compatibility.
#' @param quiet Logical. A single `TRUE` or `FALSE` value. If `TRUE`, suppresses
#'   printed summaries from individual check functions.
#'
#' @return For a data frame input, a data frame containing check results with
#'   columns `id`, `item`, `value`, `type`, and `group`. For a `canregs` object,
#'   either a combined data frame or a list of data frames is returned depending
#'   on `collapse`.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   id = c("410102198001011234", "410102198001011235"),
#'   sex = c(1, 2),
#'   birthda = c("1980-01-01", "1980-01-01"),
#'   inciden = c("2020-01-01", "2019-12-31"),
#'   topo = c("C61.9", "C50.9"),
#'   morp = c(8140, 8500),
#'   beha = c(3, 3),
#'   grad = c(1, 2),
#'   basi = c(7, 7),
#'   stringsAsFactors = FALSE
#' )
#'
#' cr_check(df, quiet = TRUE)
#'
cr_check <- function(
  x,
  schema = "ncc",
  header_check = FALSE,
  collapse = TRUE,
  as_tibble = FALSE,
  quiet = TRUE
) {
  UseMethod("cr_check", x)
}

#' Conduct quality checks for cancer registry data
#'
#' @rdname cr_check
#' @method cr_check data.frame
#' @return A data frame containing check results.
#' @export
#'
#'
cr_check.data.frame <- function(
  x,
  schema = "ncc",
  header_check = FALSE,
  collapse = TRUE,
  as_tibble = FALSE,
  quiet = TRUE
) {
  check_data_frame(x)
  check_flag(header_check, "header_check")
  check_flag(collapse, "collapse")
  check_flag(as_tibble, "as_tibble")
  check_flag(quiet, "quiet")

  vars_present <- tolower(names(x))

  if (!schema %in% names(SchemaHeader)) {
    stop(
      "`schema` must be one of: ",
      paste(names(SchemaHeader), collapse = ", "),
      call. = FALSE
    )
  }

  if (
    !"age" %in% vars_present && all(c("birthda", "inciden") %in% vars_present)
  ) {
    x$age <- calc_registry_age(
      get_column_ci(x, "birthda"),
      get_column_ci(x, "inciden")
    )
    vars_present <- tolower(names(x))
  }

  results <- list()
  for (rule in Req_Vars_Func) {
    if (all(rule$req_vars %in% vars_present)) {
      args <- list(quiet = quiet)
      if (!is.null(rule$extra)) {
        args <- c(args, rule$extra)
      }
      col_args <- lapply(rule$req_vars, function(v) get_column_ci(x, v))
      names(col_args) <- NULL
      fn <- match.fun(rule$func)
      results[[rule$name]] <- do.call(fn, c(col_args, args))
    }
  }

  header_result <- NULL
  if (header_check) {
    header_result <- check_header(x, schema = schema)
    header <- print(header_result)
    header$id <- as.character(header$id)
  }

  items <- c(
    "id",
    "sex",
    "age",
    "topo",
    "morp",
    "beha",
    "grad",
    "basi",
    "icd10"
  )
  items_present <- items[items %in% vars_present]
  single_items <- lapply(items_present, function(item) {
    check_single_item(
      get_column_ci(x, item),
      var_name = item,
      allow_missing = NULL,
      quiet = TRUE
    )
  })
  res <- bind_check_results(append(results, single_items))
  res <- map_check_ids(res, x)
  rownames(res) <- NULL
  if (header_check) {
    res <- rbind(header, res)
    rownames(res) <- NULL
  }
  res <- maybe_as_tibble(res, as_tibble = as_tibble)
  res
}

#' Conduct quality checks for cancer registry data
#'
#' @rdname cr_check
#' @method cr_check canreg
#'
#' @return A data frame with check results.
#' @export
#'
cr_check.canreg <- function(
  x,
  schema = "ncc",
  header_check = FALSE,
  collapse = TRUE,
  as_tibble = FALSE,
  quiet = TRUE
) {
  check_flag(as_tibble, "as_tibble")

  if (is.null(x$FBcases)) {
    stop("`x` must contain a `FBcases` data frame.", call. = FALSE)
  }
  check_data_frame(x$FBcases, "x$FBcases")

  fbcases <- x$FBcases
  res <- cr_check(
    fbcases,
    schema = schema,
    header_check = header_check,
    collapse = collapse,
    as_tibble = FALSE,
    quiet = quiet
  )
  if (collapse) {
    areacode <- if (is.null(x$areacode)) NA_character_ else x$areacode
    res <- cbind(areacode = rep_len(areacode, nrow(res)), res)
    res <- maybe_as_tibble(res, as_tibble = as_tibble)
  }
  res
}

#' Conduct quality checks for cancer registry data from multiple registries
#'
#' Applies \code{cr_check()} to each element of a \code{canregs} object.
#'
#' @rdname cr_check
#' @method cr_check canregs
#'
#' @param x An object of class \code{canregs}, typically a list of
#'   cancer registry datasets.
#'
#' @return An object of class \code{checks}, which is a list of data frames
#'   containing quality check results for each registry.
#'
#' @export
#'
cr_check.canregs <- function(
  x,
  schema = "ncc",
  header_check = FALSE,
  collapse = TRUE,
  as_tibble = FALSE,
  quiet = TRUE
) {
  check_flag(as_tibble, "as_tibble")

  res <- lapply(
    x,
    cr_check.canreg,
    schema = schema,
    header_check = header_check,
    collapse = collapse,
    as_tibble = FALSE,
    quiet = quiet
  )
  if (collapse) {
    res <- do.call(rbind, res)
    rownames(res) <- NULL
    res <- maybe_as_tibble(res, as_tibble = as_tibble)
  }
  if (collapse && !as_tibble) {
    class(res) <- c("check", "data.frame")
  } else if (!collapse) {
    class(res) <- c("checks", "list")
  }

  res
}

#' Create an empty data frame for printed check results
#'
#' @noRd
empty_check_result <- function() {
  data.frame(
    id = character(0),
    item = character(0),
    value = character(0),
    type = character(0),
    group = character(0),
    stringsAsFactors = FALSE
  )
}

#' Bind printed check results safely
#'
#' @noRd
bind_check_results <- function(x) {
  if (length(x) == 0L) {
    return(empty_check_result())
  }

  out <- lapply(x, print)
  out <- out[vapply(out, nrow, integer(1)) > 0L]

  if (length(out) == 0L) {
    return(empty_check_result())
  }

  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

#' Map row positions in check results to registry identifiers
#'
#' @noRd
map_check_ids <- function(res, x) {
  if (nrow(res) == 0L) {
    return(res)
  }

  registr <- get_column_ci(x, "registr")
  id <- get_column_ci(x, "id")
  id_source <- if (!is.null(registr)) registr else id

  if (is.null(id_source)) {
    return(res)
  }

  pos <- suppressWarnings(as.integer(res$id))
  valid <- !is.na(pos) & pos >= 1L & pos <= length(id_source)
  res$id[valid] <- as.character(id_source[pos[valid]])
  res
}
