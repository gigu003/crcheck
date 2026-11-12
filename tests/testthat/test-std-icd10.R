test_that("std_icd10 standardizes common ICD-10 formats", {
  expect_equal(std_icd10("C50.9"), "C50.9")
  expect_equal(std_icd10("C34.1"), "C34.1")
  expect_equal(std_icd10("C509"), "C50.9")
  expect_equal(std_icd10("D05.1"), "D05.1")
})

test_that("std_icd10 handles 2-digit numeric parts: prefers xx9 over xx0", {
  expect_equal(std_icd10("C50"), "C50.9")
})

test_that("std_icd10 handles NA and invalid inputs", {
  expect_equal(std_icd10(NA_character_), NA_character_)
  expect_equal(std_icd10(""), NA_character_)
  expect_equal(std_icd10("XYZ"), NA_character_)
  expect_equal(std_icd10("12345"), NA_character_)
})

test_that("std_icd10 keeps output length", {
  res <- std_icd10(c("C50.9", "D05.1", NA, "C34"))
  expect_length(res, 4L)
  expect_type(res, "character")
})

test_that("std_icd10 falls back to xx0 when xx9 is invalid", {
  # C64.9 is not a valid ICD-10; fallback C64.0 -> "C64" should be used.
  expect_equal(std_icd10("C64"), "C64")
  # C58.9 is not a valid ICD-10; fallback C58.0 -> "C58" should be used.
  expect_equal(std_icd10("C58"), "C58")
})

test_that("std_icd10 errors on non-character input", {
  expect_error(std_icd10(1), "must be a character")
})
