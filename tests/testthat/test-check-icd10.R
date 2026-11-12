test_that("check_icd10 returns inconsistent ICD-10 records", {
  res <- check_icd10(
    topo = c("C509", "C180"),
    morp = c("8500/3", 8000),
    beha = c(3, 0),
    icd10 = c("C34.9", "D120"),
    quiet = TRUE
  )

  expect_s3_class(res, "check")
  expect_equal(res$check, 1L)
  expect_equal(res$check_item, "Topo/Morp/Beha/ICD10")
  expect_equal(res$data, "C50.9/8500/3/C34.9")
  expect_equal(res$type, "E-ICD10")
  expect_equal(res$group, "G-01")
})

test_that("check_icd10 accepts equivalent common code formats", {
  res <- check_icd10("C509", "8500/3", "3", "C509", quiet = TRUE)

  expect_length(res$check, 0L)
  expect_equal(nrow(print(res)), 0L)
})

test_that("check_icd10 skips records that cannot be compared", {
  res <- check_icd10(
    topo = c(NA, "C99.9", "C50.9"),
    morp = c(8500, 9999, 8500),
    beha = c(3, 9, 3),
    icd10 = c("C50.9", "C50.9", NA),
    quiet = TRUE
  )

  expect_length(res$check, 0L)
})

test_that("check_icd10 validates input lengths and quiet", {
  expect_snapshot(
    check_icd10(c("C50.9", "C18.0"), 8500, 3, "C50.9", quiet = TRUE),
    error = TRUE
  )
  expect_snapshot(
    check_icd10("C50.9", 8500, 3, "C50.9", quiet = NA),
    error = TRUE
  )
})

test_that("check_icd10 integrates with cr_check", {
  x <- data.frame(
    id = c("case-a", "case-b"),
    topo = c("C50.9", "C18.0"),
    morp = c(8500, 8000),
    beha = c(3, 0),
    icd10 = c("C34.9", "D12.0"),
    stringsAsFactors = FALSE
  )

  res <- cr_check(x, quiet = TRUE)
  res <- res[res$type == "E-ICD10", , drop = FALSE]

  expect_equal(nrow(res), 1L)
  expect_equal(res$id, "case-a")
  expect_equal(res$item, "Topo/Morp/Beha/ICD10")
  expect_equal(res$value, "C50.9/8500/3/C34.9")
  expect_equal(res$group, "G-01")
})
