rule_build_script <- testthat::test_path(
  "..",
  "..",
  "data-raw",
  "create_CheckRules.R"
)

test_that("rule build sources generators and saves internal data directly", {
  if (!file.exists(rule_build_script)) {
    expect_identical(file.exists(rule_build_script), FALSE)
    return()
  }

  script <- readLines(rule_build_script, warn = FALSE)
  source_calls <- grep('^source\\("data-raw/.+[.]R"\\)$', script, value = TRUE)

  expect_identical(length(source_calls), 17L)
  expect_identical(
    any(grepl("Example_data.R", source_calls, fixed = TRUE)),
    FALSE
  )
  expect_identical(
    any(grepl("ENCR_MultRule.R", source_calls, fixed = TRUE)),
    FALSE
  )
  expect_identical(any(grepl("usethis::use_data(", script, fixed = TRUE)), TRUE)
  expect_identical(any(grepl("internal = TRUE", script, fixed = TRUE)), TRUE)
  expect_identical(any(grepl("overwrite = TRUE", script, fixed = TRUE)), TRUE)
  expect_identical(any(grepl('compress = "xz"', script, fixed = TRUE)), TRUE)
  expect_identical(any(grepl("version = 3", script, fixed = TRUE)), TRUE)
  expect_identical(any(grepl("build_check_rules", script, fixed = TRUE)), FALSE)
  expect_identical(any(grepl("RuleManifest", script, fixed = TRUE)), FALSE)
  expect_identical(any(grepl("rule-sources", script, fixed = TRUE)), FALSE)
  expect_identical(any(grepl("rule-modules", script, fixed = TRUE)), FALSE)
  expect_identical(any(grepl("sha256", script, ignore.case = TRUE)), FALSE)
})
