#' Identify duplicate person records
#'
#' Estimates whether pairs of cancer-registry records refer to the same person,
#' then forms strict duplicate groups in which every pair of records reaches the
#' matching threshold. Probabilities estimated without labelled training data
#' are conditional on the candidate-generation rules and should be reviewed
#' before records are merged.
#'
#' @param x A data frame containing cancer-registry records.
#' @param record_id Optional column containing a business record identifier.
#' @param id Column containing an 18-character Chinese resident identity number.
#' @param name Column containing the person's name.
#' @param sex Column containing sex.
#' @param birthda Column containing date of birth.
#' @param address Optional column containing address.
#' @param phone Optional column containing telephone number.
#' @param blocking Character vector of blocking strategies. The default uses all
#'   applicable strategies: `"valid_id"`, `"name_birthyear"`,
#'   `"birthdate_sex_name_initial"`, and `"phone"`.
#' @param match_threshold Probability at or above which a pair is classified as
#'   the same person.
#' @param review_threshold Probability at or above which a non-match is retained
#'   for review.
#' @param keep_pairs Pair decisions to retain in the returned `pairs` table.
#' @param max_pairs Maximum number of candidate and complete-link closure pairs.
#' @param n_cores Number of parallel workers used for field comparison.
#' @param quiet If `FALSE`, print a summary of the result.
#'
#' @return An object of class `duplicate_person_result`, containing `groups`,
#'   `pairs`, and a compact `model` diagnostic summary. `groups` has one row per
#'   record in a duplicate group. `pairs` contains pair-level probabilities and
#'   comparison evidence.
#'
#' @details
#' Exact agreement between two valid resident identity numbers is treated as
#' deterministic evidence with probability 1. Two different valid identity
#' numbers receive probability 0. Other pairs are scored with an unsupervised
#' Fellegi-Sunter model fitted by [reclin2::problink_em()]. Chinese names use
#' normalized Levenshtein similarity and addresses use q-gram cosine
#' similarity from [stringdist::stringsim()].
#'
#' Grouping is deliberately stricter than graph connectivity. Before two groups
#' are merged, every cross-group record pair is scored and must reach
#' `match_threshold`. This prevents an A-B and B-C chain from automatically
#' merging A, B, and C when A-C is weak.
#'
#' @export
#'
#' @examples
#' people <- data.frame(
#'   record = paste0("R", 1:4),
#'   id = rep("11010519491231002X", 4),
#'   name = rep("\u5f20\u4e09", 4),
#'   sex = rep(1, 4),
#'   birthda = rep("1949-12-31", 4),
#'   stringsAsFactors = FALSE
#' )
#'
#' result <- check_duplicate_person(
#'   people,
#'   record_id = "record",
#'   quiet = TRUE
#' )
#' result$groups
check_duplicate_person <- function(
  x,
  record_id = NULL,
  id = "id",
  name = "name",
  sex = "sex",
  birthda = "birthda",
  address = NULL,
  phone = NULL,
  blocking = NULL,
  match_threshold = 0.95,
  review_threshold = 0.50,
  keep_pairs = c("same_person", "review"),
  max_pairs = 5e6,
  n_cores = 1L,
  quiet = FALSE
) {
  check_data_frame(x)
  check_single_string(record_id, "record_id", allow_null = TRUE)
  check_single_string(id, "id")
  check_single_string(name, "name")
  check_single_string(sex, "sex")
  check_single_string(birthda, "birthda")
  check_single_string(address, "address", allow_null = TRUE)
  check_single_string(phone, "phone", allow_null = TRUE)
  check_flag(quiet, "quiet")
  duplicate_check_thresholds(match_threshold, review_threshold)
  duplicate_check_positive_integer(n_cores, "n_cores")
  duplicate_check_positive_number(max_pairs, "max_pairs")

  decisions <- c("same_person", "review", "different_person")
  if (
    !is.character(keep_pairs) ||
      length(keep_pairs) < 1L ||
      anyNA(keep_pairs) ||
      any(!keep_pairs %in% decisions)
  ) {
    stop(
      "`keep_pairs` must contain one or more of: ",
      paste(decisions, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  keep_pairs <- unique(keep_pairs)

  required <- c(id, name, sex, birthda)
  optional <- c(record_id, address, phone)
  check_required_columns_ci(
    x,
    c(
      required,
      optional[
        !vapply(
          optional,
          is.null,
          logical(1)
        )
      ]
    )
  )

  data <- duplicate_prepare_data(
    x,
    record_id = record_id,
    id = id,
    name = name,
    sex = sex,
    birthda = birthda,
    address = address,
    phone = phone
  )
  blocking <- duplicate_resolve_blocking(blocking, data)
  candidates <- duplicate_generate_candidates(data, blocking, max_pairs)

  if (nrow(candidates) == 0L) {
    result <- duplicate_empty_result(
      data,
      blocking,
      match_threshold,
      review_threshold
    )
    if (!quiet) {
      print(result)
    }
    return(result)
  }

  comparisons <- duplicate_compare_pairs(
    data,
    candidates$row_1,
    candidates$row_2,
    n_cores = n_cores
  )
  scored <- duplicate_score_comparisons(
    comparisons,
    match_threshold = match_threshold,
    review_threshold = review_threshold
  )
  pairs <- cbind(candidates, scored$comparisons)

  closure <- duplicate_complete_link_closure(
    pairs,
    data,
    model = scored$model,
    model_features = scored$features,
    match_threshold = match_threshold,
    review_threshold = review_threshold,
    max_pairs = max_pairs,
    n_cores = n_cores
  )
  pairs <- closure$pairs

  grouped <- duplicate_complete_link(
    pairs,
    n_records = nrow(data),
    threshold = match_threshold
  )
  pairs$grouping_status <- grouped$status
  groups <- duplicate_build_groups(data, pairs, grouped$membership)
  pairs <- duplicate_build_pair_output(data, pairs, keep_pairs)

  model_summary <- duplicate_model_summary(
    scored,
    blocking = blocking,
    candidate_pairs = nrow(candidates),
    closure_pairs = closure$n_added,
    match_threshold = match_threshold,
    review_threshold = review_threshold
  )
  result <- structure(
    list(groups = groups, pairs = pairs, model = model_summary),
    class = "duplicate_person_result"
  )

  if (!quiet) {
    print(result)
  }
  result
}

#' Print a duplicate-person result
#'
#' @param x A `duplicate_person_result` object.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @export
print.duplicate_person_result <- function(x, ...) {
  n_groups <- length(unique(x$groups$duplicate_group))
  n_records <- nrow(x$groups)
  max_size <- if (n_records == 0L) 0L else max(x$groups$group_size)
  n_review <- sum(x$pairs$decision == "review")

  cat("Duplicate person check\n")
  cat("  Duplicate groups:", n_groups, "\n")
  cat("  Records in groups:", n_records, "\n")
  cat("  Largest group:", max_size, "\n")
  cat("  Review pairs:", n_review, "\n")
  cat("  Model:", x$model$method, "\n")
  cat("  Converged:", x$model$converged, "\n")
  invisible(x)
}

duplicate_check_thresholds <- function(match_threshold, review_threshold) {
  valid <- is.numeric(match_threshold) &&
    length(match_threshold) == 1L &&
    !is.na(match_threshold) &&
    is.finite(match_threshold) &&
    is.numeric(review_threshold) &&
    length(review_threshold) == 1L &&
    !is.na(review_threshold) &&
    is.finite(review_threshold) &&
    review_threshold >= 0 &&
    review_threshold < match_threshold &&
    match_threshold <= 1

  if (!valid) {
    stop(
      "`review_threshold` and `match_threshold` must satisfy ",
      "0 <= review_threshold < match_threshold <= 1.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

duplicate_check_positive_integer <- function(x, arg) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x < 1 ||
      x != as.integer(x)
  ) {
    stop("`", arg, "` must be a positive integer.", call. = FALSE)
  }
  invisible(TRUE)
}

duplicate_check_positive_number <- function(x, arg) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x < 1
  ) {
    stop("`", arg, "` must be a positive number.", call. = FALSE)
  }
  invisible(TRUE)
}

duplicate_prepare_data <- function(
  x,
  record_id,
  id,
  name,
  sex,
  birthda,
  address,
  phone
) {
  n <- nrow(x)
  id_raw <- get_column_ci(x, id)
  name_raw <- get_column_ci(x, name)
  sex_raw <- get_column_ci(x, sex)
  birthda_raw <- get_column_ci(x, birthda)
  address_raw <- if (is.null(address)) {
    rep(NA_character_, n)
  } else {
    get_column_ci(x, address)
  }
  phone_raw <- if (is.null(phone)) {
    rep(NA_character_, n)
  } else {
    get_column_ci(x, phone)
  }

  id_std <- toupper(trimws(as.character(id_raw)))
  id_std[id_std %in% c("", "NA", "NULL")] <- NA_character_
  name_std <- duplicate_normalize_text(name_raw)
  address_std <- duplicate_normalize_text(address_raw)
  sex_std <- trimws(as.character(sex_raw))
  sex_std[sex_std %in% c("", "NA", "NULL")] <- NA_character_
  birthda_std <- parse_registry_date(birthda_raw, arg = birthda)
  phone_std <- gsub("[^0-9]", "", as.character(phone_raw))
  phone_std[is.na(phone_raw) | nchar(phone_std) == 0L] <- NA_character_

  result <- data.frame(
    row = seq_len(n),
    record_id_raw = if (is.null(record_id)) {
      rep(NA_character_, n)
    } else {
      as.character(get_column_ci(x, record_id))
    },
    id_raw = as.character(id_raw),
    name_raw = as.character(name_raw),
    sex_raw = as.character(sex_raw),
    birthda_raw = as.character(birthda_raw),
    address_raw = as.character(address_raw),
    phone_raw = as.character(phone_raw),
    id_std = id_std,
    id_valid = duplicate_valid_chinese_id(id_std),
    name_std = name_std,
    sex_std = sex_std,
    birthda_std = birthda_std,
    birthyear = format(birthda_std, "%Y"),
    name_initial = ifelse(
      is.na(name_std),
      NA_character_,
      substr(name_std, 1L, 1L)
    ),
    address_std = address_std,
    phone_std = phone_std,
    phone_suffix = ifelse(
      is.na(phone_std),
      NA_character_,
      substr(phone_std, pmax(1L, nchar(phone_std) - 7L), nchar(phone_std))
    ),
    stringsAsFactors = FALSE
  )
  attr(result, "has_record_id") <- !is.null(record_id)
  attr(result, "has_address") <- !is.null(address)
  attr(result, "has_phone") <- !is.null(phone)
  result
}

duplicate_normalize_text <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  idx <- !is.na(x)
  x[idx] <- toupper(gsub(
    paste0(
      "[[:space:][:punct:]",
      "\uff0c\u3002\u00b7\u2022\u3001\uff1b\uff1a\uff01\uff1f",
      "\uff08\uff09\u3010\u3011\u300a\u300b\u201c\u201d\u2018\u2019",
      "\u3000]+"
    ),
    "",
    x[idx]
  ))
  x[nchar(x) == 0L] <- NA_character_
  x
}

duplicate_valid_chinese_id <- function(x) {
  valid <- !is.na(x) & grepl("^[0-9]{17}[0-9X]$", x)
  if (!any(valid)) {
    return(rep(FALSE, length(x)))
  }

  birth <- suppressWarnings(as.Date(substr(x[valid], 7L, 14L), "%Y%m%d"))
  valid_positions <- which(valid)
  valid[valid_positions[is.na(birth)]] <- FALSE
  valid_positions <- which(valid)
  if (length(valid_positions) == 0L) {
    return(valid)
  }

  weights <- c(7, 9, 10, 5, 8, 4, 2, 1, 6, 3, 7, 9, 10, 5, 8, 4, 2)
  check_map <- c("1", "0", "X", "9", "8", "7", "6", "5", "4", "3", "2")
  expected <- vapply(
    x[valid_positions],
    function(value) {
      digits <- as.integer(strsplit(substr(value, 1L, 17L), "", fixed = TRUE)[[
        1L
      ]])
      check_map[(sum(digits * weights) %% 11L) + 1L]
    },
    character(1)
  )
  valid[valid_positions] <- substr(x[valid_positions], 18L, 18L) == expected
  valid
}

duplicate_resolve_blocking <- function(blocking, data) {
  available <- c(
    "valid_id",
    "name_birthyear",
    "birthdate_sex_name_initial"
  )
  if (isTRUE(attr(data, "has_phone"))) {
    available <- c(available, "phone")
  }
  if (is.null(blocking)) {
    return(available)
  }
  if (
    !is.character(blocking) ||
      length(blocking) < 1L ||
      anyNA(blocking) ||
      any(!blocking %in% available)
  ) {
    stop(
      "`blocking` must contain one or more applicable strategies: ",
      paste(available, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  unique(blocking)
}

duplicate_block_key <- function(data, rule) {
  if (rule == "valid_id") {
    return(ifelse(data$id_valid, data$id_std, NA_character_))
  }
  if (rule == "name_birthyear") {
    complete <- !is.na(data$name_std) & !is.na(data$birthyear)
    return(ifelse(
      complete,
      paste(data$name_std, data$birthyear, sep = "\r"),
      NA_character_
    ))
  }
  if (rule == "birthdate_sex_name_initial") {
    birth <- format(data$birthda_std, "%Y-%m-%d")
    complete <- !is.na(birth) & !is.na(data$sex_std) & !is.na(data$name_initial)
    return(ifelse(
      complete,
      paste(birth, data$sex_std, data$name_initial, sep = "\r"),
      NA_character_
    ))
  }
  if (rule == "phone") {
    return(data$phone_suffix)
  }
  stop("Unknown blocking strategy: ", rule, call. = FALSE)
}

duplicate_generate_candidates <- function(data, blocking, max_pairs) {
  out <- vector("list", length(blocking))
  for (i in seq_along(blocking)) {
    rule <- blocking[i]
    key <- duplicate_block_key(data, rule)
    use <- !is.na(key) & key != ""
    counts <- table(key[use])
    estimated <- sum(counts * (counts - 1) / 2)
    if (estimated > max_pairs) {
      stop(
        "Blocking strategy `",
        rule,
        "` would generate ",
        estimated,
        " pairs, exceeding `max_pairs` (",
        max_pairs,
        "). ",
        "Use stricter blocking or increase `max_pairs`.",
        call. = FALSE
      )
    }
    if (estimated == 0) {
      next
    }

    subset <- data.frame(.row = data$row[use], .block_key = key[use])
    generated <- reclin2::pair_blocking(
      x = subset,
      on = ".block_key",
      deduplication = TRUE,
      add_xy = FALSE
    )
    out[[i]] <- data.frame(
      row_1 = subset$.row[generated$.x],
      row_2 = subset$.row[generated$.y],
      blocking_rule = rule,
      stringsAsFactors = FALSE
    )
  }

  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0L) {
    return(data.frame(
      row_1 = integer(0),
      row_2 = integer(0),
      blocking_rule = character(0)
    ))
  }

  combined <- do.call(rbind, out)
  lo <- pmin(combined$row_1, combined$row_2)
  hi <- pmax(combined$row_1, combined$row_2)
  key <- paste(lo, hi, sep = "\r")
  rules <- vapply(
    split(combined$blocking_rule, key),
    function(value) {
      paste(sort(unique(value)), collapse = ",")
    },
    character(1)
  )
  unique_keys <- names(rules)
  parts <- strsplit(unique_keys, "\r", fixed = TRUE)
  result <- data.frame(
    row_1 = as.integer(vapply(parts, `[`, character(1), 1L)),
    row_2 = as.integer(vapply(parts, `[`, character(1), 2L)),
    blocking_rule = unname(rules),
    stringsAsFactors = FALSE
  )
  result <- result[order(result$row_1, result$row_2), , drop = FALSE]
  rownames(result) <- NULL
  if (nrow(result) > max_pairs) {
    stop(
      "Candidate generation produced ",
      nrow(result),
      " unique pairs, exceeding `max_pairs` (",
      max_pairs,
      ").",
      call. = FALSE
    )
  }
  result
}

duplicate_compare_pairs <- function(data, row_1, row_2, n_cores = 1L) {
  n <- length(row_1)
  if (n == 0L) {
    return(duplicate_compare_chunk(data, row_1, row_2))
  }
  if (n_cores == 1L || n < 1000L) {
    return(duplicate_compare_chunk(data, row_1, row_2))
  }

  workers <- min(as.integer(n_cores), n)
  chunks <- split(seq_len(n), rep_len(seq_len(workers), n))
  cluster <- parallel::makeCluster(workers)
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  parallel::clusterExport(
    cluster,
    c(
      "duplicate_compare_chunk",
      "duplicate_string_similarity",
      "duplicate_equal_or_na"
    ),
    envir = environment(duplicate_compare_pairs)
  )
  pieces <- parallel::parLapply(
    cluster,
    chunks,
    function(idx, data, r1, r2) {
      duplicate_compare_chunk(data, r1[idx], r2[idx])
    },
    data = data,
    r1 = row_1,
    r2 = row_2
  )
  do.call(rbind, pieces)
}

duplicate_compare_chunk <- function(data, row_1, row_2) {
  id_valid_1 <- data$id_valid[row_1]
  id_valid_2 <- data$id_valid[row_2]
  both_valid <- id_valid_1 & id_valid_2
  id_agree <- rep(NA, length(row_1))
  id_agree[both_valid] <- data$id_std[row_1[both_valid]] ==
    data$id_std[row_2[both_valid]]

  name_similarity <- duplicate_string_similarity(
    data$name_std[row_1],
    data$name_std[row_2],
    method = "lv"
  )
  address_similarity <- duplicate_string_similarity(
    data$address_std[row_1],
    data$address_std[row_2],
    method = "cosine",
    q = 2
  )
  sex_agree <- duplicate_equal_or_na(data$sex_std[row_1], data$sex_std[row_2])
  birthdate_agree <- duplicate_equal_or_na(
    data$birthda_std[row_1],
    data$birthda_std[row_2]
  )
  phone_agree <- duplicate_equal_or_na(
    data$phone_suffix[row_1],
    data$phone_suffix[row_2]
  )

  data.frame(
    id_valid_1 = id_valid_1,
    id_valid_2 = id_valid_2,
    id_agree = id_agree,
    name_similarity = name_similarity,
    name_agree = ifelse(is.na(name_similarity), NA, name_similarity >= 2 / 3),
    sex_agree = sex_agree,
    birthdate_agree = birthdate_agree,
    address_similarity = address_similarity,
    address_agree = ifelse(
      is.na(address_similarity),
      NA,
      address_similarity >= 0.70
    ),
    phone_agree = phone_agree,
    stringsAsFactors = FALSE
  )
}

duplicate_string_similarity <- function(x, y, method, q = 1L) {
  missing <- is.na(x) | is.na(y) | x == "" | y == ""
  result <- rep(NA_real_, length(x))
  if (any(!missing)) {
    result[!missing] <- stringdist::stringsim(
      x[!missing],
      y[!missing],
      method = method,
      q = q
    )
  }
  result
}

duplicate_equal_or_na <- function(x, y) {
  missing <- is.na(x) | is.na(y)
  result <- rep(NA, length(x))
  result[!missing] <- x[!missing] == y[!missing]
  result
}

duplicate_score_comparisons <- function(
  comparisons,
  match_threshold,
  review_threshold
) {
  deterministic_same <- comparisons$id_valid_1 &
    comparisons$id_valid_2 &
    comparisons$id_agree %in% TRUE
  deterministic_different <- comparisons$id_valid_1 &
    comparisons$id_valid_2 &
    comparisons$id_agree %in% FALSE
  unresolved <- !(deterministic_same | deterministic_different)

  probability <- rep(NA_real_, nrow(comparisons))
  source <- rep(NA_character_, nrow(comparisons))
  probability[deterministic_same] <- 1
  probability[deterministic_different] <- 0
  source[deterministic_same | deterministic_different] <- "verified_id"

  fitted <- duplicate_fit_em(comparisons[unresolved, , drop = FALSE])
  if (any(unresolved)) {
    if (is.null(fitted$model)) {
      stop(
        "The unsupervised linkage model could not be fitted because the ",
        "candidate pairs contain too little comparison variation. Supply ",
        "more records or stronger identity fields.",
        call. = FALSE
      )
    }
    probability[unresolved] <- duplicate_predict_em(
      fitted$model,
      comparisons[unresolved, , drop = FALSE],
      fitted$features
    )
    source[unresolved] <- "fellegi_sunter_em"
  }

  decision <- duplicate_probability_decision(
    probability,
    match_threshold,
    review_threshold
  )
  comparisons$duplicate_probability <- probability
  comparisons$decision <- decision
  comparisons$probability_source <- source

  list(
    comparisons = comparisons,
    model = fitted$model,
    features = fitted$features,
    fitted_pairs = fitted$n,
    warnings = fitted$warnings
  )
}

duplicate_fit_em <- function(comparisons) {
  feature_candidates <- c(
    "name_agree",
    "sex_agree",
    "birthdate_agree",
    "address_agree",
    "phone_agree"
  )
  features <- feature_candidates[vapply(
    comparisons[feature_candidates],
    function(value) length(unique(value[!is.na(value)])) >= 2L,
    logical(1)
  )]
  if (length(features) == 0L) {
    return(list(
      model = NULL,
      features = character(0),
      n = 0L,
      warnings = character(0)
    ))
  }

  complete <- stats::complete.cases(comparisons[features])
  fit_data <- comparisons[complete, features, drop = FALSE]
  features <- features[vapply(
    fit_data,
    function(value) length(unique(value)) >= 2L,
    logical(1)
  )]
  if (length(features) == 0L || nrow(fit_data) < 2L) {
    return(list(
      model = NULL,
      features = character(0),
      n = 0L,
      warnings = character(0)
    ))
  }
  fit_data <- fit_data[features]
  fit_pairs <- data.table::as.data.table(data.frame(
    .x = seq_len(nrow(fit_data)),
    .y = seq_len(nrow(fit_data)),
    fit_data,
    check.names = FALSE
  ))
  class(fit_pairs) <- unique(c("pairs", class(fit_pairs)))
  formula <- stats::reformulate(features)
  warnings <- character(0)
  model <- tryCatch(
    withCallingHandlers(
      reclin2::problink_em(formula, data = fit_pairs),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      stop(
        "The `reclin2` EM model could not be fitted reliably: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (length(warnings) > 0L) {
    stop(
      "The `reclin2` EM model could not be fitted reliably: ",
      paste(unique(warnings), collapse = "; "),
      call. = FALSE
    )
  }
  list(
    model = model,
    features = features,
    n = nrow(fit_data),
    warnings = warnings
  )
}

duplicate_predict_em <- function(model, comparisons, features) {
  pairs <- data.table::as.data.table(data.frame(
    .x = seq_len(nrow(comparisons)),
    .y = seq_len(nrow(comparisons)),
    comparisons[features],
    check.names = FALSE
  ))
  class(pairs) <- unique(c("pairs", class(pairs)))
  prediction <- stats::predict(model, pairs, type = "mpost")$mpost
  if (any(!is.finite(prediction))) {
    stop(
      "The fitted linkage model produced non-finite probabilities.",
      call. = FALSE
    )
  }
  as.numeric(prediction)
}

duplicate_probability_decision <- function(probability, match, review) {
  ifelse(
    probability >= match,
    "same_person",
    ifelse(probability >= review, "review", "different_person")
  )
}

duplicate_complete_link_closure <- function(
  pairs,
  data,
  model,
  model_features,
  match_threshold,
  review_threshold,
  max_pairs,
  n_cores
) {
  same <- pairs$duplicate_probability >= match_threshold
  membership <- duplicate_connected_components(
    pairs$row_1[same],
    pairs$row_2[same],
    nrow(data)
  )
  component_rows <- split(which(membership > 0L), membership[membership > 0L])
  component_rows <- component_rows[lengths(component_rows) > 1L]
  if (length(component_rows) == 0L) {
    return(list(pairs = pairs, n_added = 0L))
  }

  needed <- do.call(
    rbind,
    lapply(component_rows, function(rows) {
      if (length(rows) < 2L) {
        return(NULL)
      }
      combo <- utils::combn(rows, 2L)
      data.frame(row_1 = combo[1L, ], row_2 = combo[2L, ])
    })
  )
  existing_key <- paste(pairs$row_1, pairs$row_2, sep = "\r")
  needed_key <- paste(needed$row_1, needed$row_2, sep = "\r")
  needed <- needed[!needed_key %in% existing_key, , drop = FALSE]
  if (nrow(needed) == 0L) {
    return(list(pairs = pairs, n_added = 0L))
  }
  if (nrow(pairs) + nrow(needed) > max_pairs) {
    stop(
      "Complete-link closure would increase the comparison set to ",
      nrow(pairs) + nrow(needed),
      " pairs, exceeding `max_pairs` (",
      max_pairs,
      ").",
      call. = FALSE
    )
  }

  comparisons <- duplicate_compare_pairs(
    data,
    needed$row_1,
    needed$row_2,
    n_cores = n_cores
  )
  same_id <- comparisons$id_valid_1 &
    comparisons$id_valid_2 &
    comparisons$id_agree %in% TRUE
  different_id <- comparisons$id_valid_1 &
    comparisons$id_valid_2 &
    comparisons$id_agree %in% FALSE
  unresolved <- !(same_id | different_id)
  probability <- rep(NA_real_, nrow(comparisons))
  source <- rep("verified_id", nrow(comparisons))
  probability[same_id] <- 1
  probability[different_id] <- 0
  if (any(unresolved)) {
    if (is.null(model)) {
      stop(
        "Complete-link grouping requires probabilities for additional pairs, ",
        "but no linkage model is available.",
        call. = FALSE
      )
    }
    probability[unresolved] <- duplicate_predict_em(
      model,
      comparisons[unresolved, , drop = FALSE],
      model_features
    )
    source[unresolved] <- "fellegi_sunter_em"
  }
  comparisons$duplicate_probability <- probability
  comparisons$decision <- duplicate_probability_decision(
    probability,
    match_threshold,
    review_threshold
  )
  comparisons$probability_source <- source
  added <- cbind(
    needed,
    blocking_rule = "complete_link_closure",
    comparisons
  )
  pairs <- rbind(pairs, added)
  pairs <- pairs[order(pairs$row_1, pairs$row_2), , drop = FALSE]
  rownames(pairs) <- NULL
  list(pairs = pairs, n_added = nrow(added))
}

duplicate_connected_components <- function(row_1, row_2, n_records) {
  parent <- seq_len(n_records)
  find_root <- function(value) {
    while (parent[value] != value) {
      value <- parent[value]
    }
    value
  }
  if (length(row_1) > 0L) {
    for (i in seq_along(row_1)) {
      root_1 <- find_root(row_1[i])
      root_2 <- find_root(row_2[i])
      if (root_1 != root_2) {
        parent[root_2] <- root_1
      }
    }
  }
  roots <- vapply(seq_len(n_records), find_root, integer(1))
  counts <- table(roots)
  duplicate_roots <- as.integer(names(counts[counts > 1L]))
  result <- integer(n_records)
  if (length(duplicate_roots) > 0L) {
    ordered <- duplicate_roots[order(vapply(
      duplicate_roots,
      function(root) min(which(roots == root)),
      integer(1)
    ))]
    for (i in seq_along(ordered)) {
      result[roots == ordered[i]] <- i
    }
  }
  result
}

duplicate_complete_link <- function(pairs, n_records, threshold) {
  membership <- seq_len(n_records)
  status <- ifelse(
    pairs$decision == "review",
    "review",
    ifelse(
      pairs$decision == "different_person",
      "different_person",
      "grouping_conflict"
    )
  )
  eligible <- which(pairs$duplicate_probability >= threshold)
  eligible <- eligible[order(
    -pairs$duplicate_probability[eligible],
    pairs$row_1[eligible],
    pairs$row_2[eligible]
  )]
  pair_key <- paste(pairs$row_1, pairs$row_2, sep = "\r")

  for (idx in eligible) {
    cluster_1 <- membership[pairs$row_1[idx]]
    cluster_2 <- membership[pairs$row_2[idx]]
    if (cluster_1 == cluster_2) {
      status[idx] <- "in_group"
      next
    }
    rows_1 <- which(membership == cluster_1)
    rows_2 <- which(membership == cluster_2)
    cross <- expand.grid(row_1 = rows_1, row_2 = rows_2)
    lo <- pmin(cross$row_1, cross$row_2)
    hi <- pmax(cross$row_1, cross$row_2)
    cross_idx <- match(paste(lo, hi, sep = "\r"), pair_key)
    can_merge <- !anyNA(cross_idx) &&
      all(pairs$duplicate_probability[cross_idx] >= threshold)
    if (can_merge) {
      membership[membership == cluster_2] <- cluster_1
      status[cross_idx] <- "in_group"
    } else {
      status[idx] <- "grouping_conflict"
    }
  }

  counts <- table(membership)
  group_clusters <- as.integer(names(counts[counts > 1L]))
  result <- integer(n_records)
  if (length(group_clusters) > 0L) {
    group_clusters <- group_clusters[order(vapply(
      group_clusters,
      function(cluster) min(which(membership == cluster)),
      integer(1)
    ))]
    for (i in seq_along(group_clusters)) {
      result[membership == group_clusters[i]] <- i
    }
  }
  list(membership = result, status = status)
}

duplicate_build_groups <- function(data, pairs, membership) {
  rows <- which(membership > 0L)
  if (length(rows) == 0L) {
    return(duplicate_empty_groups(data))
  }
  groups <- membership[rows]
  group_names <- sprintf("DP%06d", groups)
  sizes <- as.integer(table(groups)[as.character(groups)])
  minimum <- vapply(
    groups,
    function(group) {
      members <- which(membership == group)
      combos <- utils::combn(members, 2L)
      keys <- paste(combos[1L, ], combos[2L, ], sep = "\r")
      idx <- match(keys, paste(pairs$row_1, pairs$row_2, sep = "\r"))
      min(pairs$duplicate_probability[idx])
    },
    numeric(1)
  )

  result <- data.frame(
    duplicate_group = group_names,
    group_size = sizes,
    row = rows,
    stringsAsFactors = FALSE
  )
  if (isTRUE(attr(data, "has_record_id"))) {
    result$record_id <- data$record_id_raw[rows]
  }
  result$id <- data$id_raw[rows]
  result$name <- data$name_raw[rows]
  result$sex <- data$sex_raw[rows]
  result$birthda <- data$birthda_raw[rows]
  if (isTRUE(attr(data, "has_address"))) {
    result$address <- data$address_raw[rows]
  }
  if (isTRUE(attr(data, "has_phone"))) {
    result$phone <- data$phone_raw[rows]
  }
  result$minimum_pair_probability <- minimum
  result$group_decision <- "same_person"
  result <- result[order(result$duplicate_group, result$row), , drop = FALSE]
  rownames(result) <- NULL
  result
}

duplicate_empty_groups <- function(data) {
  result <- data.frame(
    duplicate_group = character(0),
    group_size = integer(0),
    row = integer(0),
    stringsAsFactors = FALSE
  )
  if (isTRUE(attr(data, "has_record_id"))) {
    result$record_id <- character(0)
  }
  result$id <- character(0)
  result$name <- character(0)
  result$sex <- character(0)
  result$birthda <- character(0)
  if (isTRUE(attr(data, "has_address"))) {
    result$address <- character(0)
  }
  if (isTRUE(attr(data, "has_phone"))) {
    result$phone <- character(0)
  }
  result$minimum_pair_probability <- numeric(0)
  result$group_decision <- character(0)
  result
}

duplicate_build_pair_output <- function(data, pairs, keep_pairs) {
  keep <- pairs$decision %in% keep_pairs
  pairs <- pairs[keep, , drop = FALSE]
  result <- data.frame(
    row_1 = pairs$row_1,
    row_2 = pairs$row_2,
    stringsAsFactors = FALSE
  )
  if (isTRUE(attr(data, "has_record_id"))) {
    result$record_id_1 <- data$record_id_raw[pairs$row_1]
    result$record_id_2 <- data$record_id_raw[pairs$row_2]
  }
  for (field in c("id", "name", "sex", "birthda")) {
    raw <- data[[paste0(field, "_raw")]]
    result[[paste0(field, "_1")]] <- raw[pairs$row_1]
    result[[paste0(field, "_2")]] <- raw[pairs$row_2]
  }
  if (isTRUE(attr(data, "has_address"))) {
    result$address_1 <- data$address_raw[pairs$row_1]
    result$address_2 <- data$address_raw[pairs$row_2]
  }
  if (isTRUE(attr(data, "has_phone"))) {
    result$phone_1 <- data$phone_raw[pairs$row_1]
    result$phone_2 <- data$phone_raw[pairs$row_2]
  }
  evidence <- c(
    "duplicate_probability",
    "decision",
    "probability_source",
    "blocking_rule",
    "grouping_status",
    "id_valid_1",
    "id_valid_2",
    "id_agree",
    "name_similarity",
    "sex_agree",
    "birthdate_agree",
    "address_similarity",
    "phone_agree"
  )
  result[evidence] <- pairs[evidence]
  result <- result[
    order(
      -result$duplicate_probability,
      result$row_1,
      result$row_2
    ),
    ,
    drop = FALSE
  ]
  rownames(result) <- NULL
  result
}

duplicate_model_summary <- function(
  scored,
  blocking,
  candidate_pairs,
  closure_pairs,
  match_threshold,
  review_threshold
) {
  if (is.null(scored$model)) {
    method <- "verified_id_only"
    converged <- NA
    m_probability <- NULL
    u_probability <- NULL
    prior_probability <- NULL
  } else {
    method <- "fellegi_sunter_em"
    converged <- TRUE
    m_probability <- scored$model$mprobs
    u_probability <- scored$model$uprobs
    prior_probability <- scored$model$p
  }
  list(
    method = method,
    converged = converged,
    conditional_on_candidates = TRUE,
    features = scored$features,
    candidate_pairs = candidate_pairs,
    fitted_pairs = scored$fitted_pairs,
    closure_pairs = closure_pairs,
    blocking = blocking,
    match_threshold = match_threshold,
    review_threshold = review_threshold,
    m_probability = m_probability,
    u_probability = u_probability,
    prior_probability = prior_probability
  )
}

duplicate_empty_result <- function(data, blocking, match, review) {
  empty_pairs <- data.frame(
    row_1 = integer(0),
    row_2 = integer(0),
    duplicate_probability = numeric(0),
    decision = character(0),
    probability_source = character(0),
    blocking_rule = character(0),
    grouping_status = character(0),
    stringsAsFactors = FALSE
  )
  structure(
    list(
      groups = duplicate_empty_groups(data),
      pairs = empty_pairs,
      model = list(
        method = "not_fitted",
        converged = NA,
        conditional_on_candidates = TRUE,
        features = character(0),
        candidate_pairs = 0L,
        fitted_pairs = 0L,
        closure_pairs = 0L,
        blocking = blocking,
        match_threshold = match,
        review_threshold = review
      )
    ),
    class = "duplicate_person_result"
  )
}
