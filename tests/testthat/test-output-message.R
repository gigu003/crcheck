read_output_messages <- function() {
  get(
    "dict_output_message",
    envir = asNamespace("crcheck"),
    inherits = FALSE
  )
}

expected_message_keys <- function() {
  keys <- list(
    c("E-MISS", "G-XX"),
    c("E-FORM", "G-XX"),
    c("E-RANGE", "G-XX"),
    c("E-ID-Date", "G-01"),
    c("E-ID-LastChar", "G-01"),
    c("E-ICD10", "G-01")
  )
  grouped <- list(
    "E-AGED" = 1:2,
    "W-AGMT" = 1:33,
    "E-SETO" = 1:2,
    "E-SEMO" = 1:2,
    "W-TOMO" = 2:71,
    "W-BDMO" = 1:9,
    "W-BETO" = 1,
    "W-MOBE" = 1:6,
    "W-BEGR" = 1,
    "W-MOGR" = 1:11,
    "E-FOUP" = 1:5,
    "W-TGBR" = 1:5,
    "E-CODA" = 1:3,
    "W-ExVars" = 1:3
  )
  for (type in names(grouped)) {
    keys <- c(
      keys,
      lapply(grouped[[type]], function(group) {
        c(type, sprintf("G-%02d", group))
      })
    )
  }
  vapply(keys, paste, character(1), collapse = "/")
}

test_that("output messages cover every check code and group", {
  messages <- read_output_messages()
  keys <- paste(messages$type, messages$group, sep = "/")

  expect_identical(
    names(messages),
    c(
      "rank",
      "check_item",
      "type",
      "group",
      "label_cn",
      "label_en"
    )
  )
  expect_identical(messages$rank, seq_len(nrow(messages)))
  expect_identical(anyDuplicated(keys), 0L)
  expect_setequal(keys, expected_message_keys())
})

test_that("output messages have complete bilingual labels", {
  messages <- read_output_messages()
  text_columns <- c("check_item", "type", "group", "label_cn", "label_en")

  for (column in text_columns) {
    expect_false(anyNA(messages[[column]]), info = column)
    expect_true(all(nzchar(trimws(messages[[column]]))), info = column)
  }
})

test_that("output messages retain the embedded dictionary contract", {
  messages <- read_output_messages()

  expect_s3_class(messages, "data.frame")
  expect_identical(nrow(messages), 159L)
})
