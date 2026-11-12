test_that("check_single_item validates arguments", {
  expect_error(
    check_single_item(1:3, var_name = c("sex", "age")),
    "var_name"
  )

  expect_error(
    check_single_item(1:3, var_name = "unknown"),
    "var_name"
  )

  expect_error(
    check_single_item(1:3, var_name = "sex", allow_missing = "yes"),
    "allow_missing"
  )

  expect_error(
    check_single_item(1:3, var_name = "sex", quiet = NA),
    "quiet"
  )
})

test_that("check_single_item handles empty strings consistently as missing", {
  res <- check_single_item(
    c("1", "", "NULL"),
    var_name = "sex",
    allow_missing = FALSE,
    quiet = TRUE
  )

  expect_equal(res$check, 2:3)
  expect_true(all(res$type == "E-MISS"))
})

test_that("check_single_item respects allow_missing for missing values", {
  res <- check_single_item(
    c("1", "", "NULL", NA),
    var_name = "sex",
    allow_missing = TRUE,
    quiet = TRUE
  )

  expect_equal(length(res$check), 0L)
})

test_that("check_single_item keeps icd10 as a supported special variable", {
  res <- check_single_item(
    c("C50.9", NA),
    var_name = "icd10",
    allow_missing = TRUE,
    quiet = TRUE
  )

  expect_s3_class(res, "check")
})
