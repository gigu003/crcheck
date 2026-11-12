test_that("icdo3_to_icd10 uses topo-morp-beha rules first", {
  expect_equal(icdo3_to_icd10("C80.9", 8124, 0), "D12.9")
})

test_that("icdo3_to_icd10 falls back to morp-beha rules", {
  expect_equal(icdo3_to_icd10("C22.0", 8170, 3), "C22.0")
})

test_that("icdo3_to_icd10 falls back to topo-beha rules", {
  expect_equal(icdo3_to_icd10("C18.0", 8000, 0), "D12.0")
})

test_that("icdo3_to_icd10 standardizes common input formats", {
  expect_equal(icdo3_to_icd10("C509", "8500/3", "3"), "C50.9")
  expect_equal(icdo3_to_icd10("509", "8500", 3), "C50.9")
})

test_that("icdo3_to_icd10 handles missing and unmatched values", {
  expect_equal(icdo3_to_icd10(NA, 8500, 3), NA_character_)
  expect_equal(
    icdo3_to_icd10("", 8500, 3, unmatched = "not found"),
    "not found"
  )
  expect_equal(
    icdo3_to_icd10("C99.9", 9999, 9, unmatched = "not found"),
    "not found"
  )
})

test_that("icdo3_to_icd10 keeps output length", {
  res <- icdo3_to_icd10(
    topo = c("C80.9", "C18.0", NA),
    morp = c(8124, 8000, 8500),
    beha = c(0, 0, 3)
  )

  expect_type(res, "character")
  expect_length(res, 3L)
})

test_that("icdo3_to_icd10 validates argument length and unmatched", {
  expect_error(icdo3_to_icd10(c("C50.9", "C18.0"), 8500, 3), "same length")
  expect_error(
    icdo3_to_icd10("C50.9", 8500, 3, unmatched = c("a", "b")),
    "unmatched"
  )
})
