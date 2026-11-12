test_that("check_multiple_primary detects different primary sites", {
  df <- data.frame(
    id = c("P1", "P1"),
    topo = c("C18.0", "C34.9"),
    morp = c(8140, 8140),
    stringsAsFactors = FALSE
  )

  res <- check_multiple_primary(df, quiet = TRUE)

  expect_s3_class(res, "multiple_primary_check")
  expect_equal(nrow(res), 1L)
  expect_equal(res$rule, "MP-R3")
})

test_that("check_multiple_primary treats Table 1 site groups as one site", {
  df <- data.frame(
    id = c("P1", "P1"),
    topo = c("C01.9", "C02.9"),
    morp = c(8070, 8071),
    stringsAsFactors = FALSE
  )

  res <- check_multiple_primary(df, quiet = TRUE)

  expect_equal(nrow(res), 0L)
})

test_that("check_multiple_primary detects different morphology groups in same site", {
  df <- data.frame(
    id = c("P1", "P1"),
    topo = c("C50.9", "C50.9"),
    morp = c(8140, 8070),
    stringsAsFactors = FALSE
  )

  res <- check_multiple_primary(df, quiet = TRUE)

  expect_equal(nrow(res), 1L)
  expect_equal(res$rule, "MP-R4.2")
})

test_that("check_multiple_primary ignores non-specific morphology with specific morphology", {
  df <- data.frame(
    id = c("P1", "P1"),
    topo = c("C50.9", "C50.9"),
    morp = c(8000, 8140),
    stringsAsFactors = FALSE
  )

  res <- check_multiple_primary(df, quiet = TRUE)

  expect_equal(nrow(res), 0L)
})

test_that("check_multiple_primary counts systemic cancers once per individual", {
  df <- data.frame(
    id = c("P1", "P1"),
    topo = c("C42.1", "C77.9"),
    morp = c(9861, 9670),
    stringsAsFactors = FALSE
  )

  res <- check_multiple_primary(df, quiet = TRUE)

  expect_equal(nrow(res), 0L)
})

test_that("check_multiple_primary can apply recording recommendations", {
  df <- data.frame(
    id = c("P1", "P1"),
    topo = c("C18.0", "C18.7"),
    morp = c(8140, 8140),
    stringsAsFactors = FALSE
  )

  res_default <- check_multiple_primary(df, quiet = TRUE)
  res_record <- check_multiple_primary(
    df,
    record_recommendations = TRUE,
    quiet = TRUE
  )

  expect_equal(nrow(res_default), 0L)
  expect_equal(nrow(res_record), 1L)
  expect_equal(res_record$rule, "MP-REC2")
})

test_that("check_multiple_primary validates inputs", {
  df <- data.frame(
    id = "P1",
    topo = "C50.9",
    morp = 8140,
    stringsAsFactors = FALSE
  )

  expect_error(
    check_multiple_primary(df, id = "missing", quiet = TRUE),
    "missing"
  )
  expect_error(check_multiple_primary(df, quiet = NA), "quiet")
  expect_error(
    check_multiple_primary(df, topo = c("topo", "site"), quiet = TRUE),
    "topo"
  )
})

test_that("show_rules displays multiple primary rule groups", {
  topo_rules <- show_rules("mult_topo")
  morp_rules <- show_rules("mult_morp")

  expect_equal(nrow(topo_rules), 8L)
  expect_equal(nrow(morp_rules), 17L)
  expect_true(all(
    c("group", "topo", "simultaneous_code") %in% names(topo_rules)
  ))
  expect_true(all(
    c("group", "morp", "nonspecific", "systemic") %in% names(morp_rules)
  ))
})
