test_that("parse_registry_date supports common registry date formats", {
  expect_equal(
    parse_registry_date(c("1960-08-24", "19600824", "1960/08/24")),
    as.Date(rep("1960-08-24", 3))
  )
})

test_that("calc_registry_age supports common registry date formats", {
  expect_equal(
    calc_registry_age(c("1960-08-24", "19600824"), c("2024-09-10", "20240910")),
    c(64, 64)
  )
})

test_that("check_equal_length reports length mismatches", {
  expect_error(
    check_equal_length(1:2, 1, arg_names = c("x", "y")),
    "same length"
  )
})
