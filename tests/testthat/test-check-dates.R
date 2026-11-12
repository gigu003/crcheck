test_that("date checks accept YYYY-MM-DD and YYYYMMDD formats", {
  res <- check_age_birth_inci(
    age = c(64, 64),
    birthda = c("1960-08-24", "19600824"),
    inciden = c("2024-09-10", "20240910"),
    quiet = TRUE
  )

  expect_s3_class(res, "check")
  expect_equal(length(res$check), 0L)
})

test_that("age and date checks accept numeric age stored as character", {
  res <- check_age_birth_inci(
    age = c("64", "064"),
    birthda = c("1960-08-24", "19600824"),
    inciden = c("2024-09-10", "20240910"),
    quiet = TRUE
  )

  expect_s3_class(res, "check")
  expect_equal(length(res$check), 0L)
})

test_that("age and date checks handle non-numeric age without arithmetic errors", {
  res <- check_age_birth_inci(
    age = c("unknown", "64"),
    birthda = c("1960-08-24", "19600824"),
    inciden = c("2024-09-10", "20240910"),
    quiet = TRUE
  )

  expect_s3_class(res, "check")
  expect_equal(res$check, 1L)
})

test_that("date order checks use parsed Date values", {
  res <- check_birth_inci_last(
    birthda = c("20200102", "2020-01-01"),
    inciden = c("20200101", "2020-01-02"),
    lastcontact = c("20200103", "2020-01-03"),
    quiet = TRUE
  )

  expect_s3_class(res, "check")
  expect_equal(res$check, 1L)
})

test_that("date checks require equal input lengths", {
  expect_error(
    check_birth_inci_last("20200101", c("20200102", "20200103"), "20200104"),
    "same length"
  )
})

test_that("follow-up checks use parsed Date values", {
  res <- check_followup(
    basi = c(0, 1),
    status = c(2, 1),
    inciden = c("20200101", "2020-01-03"),
    deathda = c("20200102", "2020-01-02"),
    lastcontact = c("20200103", "2020-01-02"),
    quiet = TRUE
  )

  expect_s3_class(res, "check")
  expect_true(all(c(1L, 2L) %in% res$check))
})
