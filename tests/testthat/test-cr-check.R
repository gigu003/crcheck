test_that("cr_check works without registr when id is available", {
  df <- data.frame(
    id = c("A", "B"),
    sex = c(1, 2),
    birthda = c("19600824", "1971-06-30"),
    inciden = c("20240910", "2023-09-11"),
    topo = c("C15.2", "C50.9"),
    morp = c(8140, 8500),
    beha = c(3, 3),
    grad = c(9, 9),
    basi = c(7, 7),
    icd10 = c("C15.2", "C50.9"),
    stringsAsFactors = FALSE
  )

  res <- cr_check(df, header_check = FALSE, quiet = TRUE)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("id", "item", "value", "type", "group") %in% names(res)))
})

test_that("cr_check validates logical flags", {
  expect_error(
    cr_check(data.frame(id = 1), header_check = NA, quiet = TRUE),
    "header_check"
  )

  expect_error(
    cr_check(data.frame(id = 1), as_tibble = NA, quiet = TRUE),
    "as_tibble"
  )
})

test_that("cr_check can return a tibble when requested", {
  skip_if_not_installed("tibble")

  df <- data.frame(
    id = c("case-a", "case-b"),
    sex = c("1", "9"),
    stringsAsFactors = FALSE
  )

  res <- cr_check(df, header_check = FALSE, as_tibble = TRUE, quiet = TRUE)

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("id", "item", "value", "type", "group") %in% names(res)))
})

test_that("cr_check returns a stable empty result when no checks are triggered", {
  df <- data.frame(
    note = c("a", "b"),
    stringsAsFactors = FALSE
  )

  res <- cr_check(df, header_check = FALSE, quiet = TRUE)

  expect_s3_class(res, "data.frame")
  expect_equal(names(res), c("id", "item", "value", "type", "group"))
  expect_equal(nrow(res), 0L)
})

test_that("cr_check maps row positions to id values", {
  df <- data.frame(
    id = c("case-a", "case-b"),
    sex = c("1", "9"),
    stringsAsFactors = FALSE
  )

  res <- cr_check(df, header_check = FALSE, quiet = TRUE)

  expect_true("case-b" %in% res$id)
})

test_that("cr_check prefers registr over id for output identifiers", {
  df <- data.frame(
    registr = c("reg-a", "reg-b"),
    id = c("case-a", "case-b"),
    sex = c("1", "9"),
    stringsAsFactors = FALSE
  )

  res <- cr_check(df, header_check = FALSE, quiet = TRUE)

  expect_true("reg-b" %in% res$id)
  expect_false("case-b" %in% res$id)
})

test_that("cr_check combines header results with ordinary results", {
  df <- data.frame(
    id = c("case-a", "case-b"),
    sex = c("1", "9"),
    extra_var = c(1, 2),
    stringsAsFactors = FALSE
  )

  res <- cr_check(df, header_check = TRUE, quiet = TRUE)

  expect_s3_class(res, "data.frame")
  expect_equal(names(res), c("id", "item", "value", "type", "group"))
  expect_true(any(grepl("Schema Header", res$item, fixed = TRUE)))
})

test_that("cr_check canreg method validates FBcases", {
  x <- structure(list(areacode = "001"), class = "canreg")

  expect_error(
    cr_check(x, quiet = TRUE),
    "FBcases"
  )

  x$FBcases <- "not a data frame"
  expect_error(
    cr_check(x, quiet = TRUE),
    "data frame"
  )
})

test_that("cr_check canreg method returns areacode when collapsed", {
  x <- structure(
    list(
      areacode = "001",
      FBcases = data.frame(
        id = c("case-a", "case-b"),
        sex = c("1", "9"),
        stringsAsFactors = FALSE
      )
    ),
    class = "canreg"
  )

  res <- cr_check(x, quiet = TRUE)

  expect_s3_class(res, "data.frame")
  expect_true("areacode" %in% names(res))
  expect_true(all(res$areacode == "001"))
})

test_that("cr_check canreg method can return a tibble when requested", {
  skip_if_not_installed("tibble")

  x <- structure(
    list(
      areacode = "001",
      FBcases = data.frame(
        id = c("case-a", "case-b"),
        sex = c("1", "9"),
        stringsAsFactors = FALSE
      )
    ),
    class = "canreg"
  )

  res <- cr_check(x, as_tibble = TRUE, quiet = TRUE)

  expect_s3_class(res, "tbl_df")
  expect_true("areacode" %in% names(res))
})

test_that("cr_check canregs method supports collapsed tibble output", {
  skip_if_not_installed("tibble")

  one_registry <- structure(
    list(
      areacode = "001",
      FBcases = data.frame(
        id = c("case-a", "case-b"),
        sex = c("1", "9"),
        stringsAsFactors = FALSE
      )
    ),
    class = "canreg"
  )
  x <- structure(list(one_registry), class = "canregs")

  res <- cr_check(x, as_tibble = TRUE, quiet = TRUE)

  expect_s3_class(res, "tbl_df")
  expect_true("areacode" %in% names(res))
})

test_that("cr_check canregs method returns a checks list when not collapsed", {
  one_registry <- structure(
    list(
      areacode = "001",
      FBcases = data.frame(
        id = c("case-a", "case-b"),
        sex = c("1", "9"),
        stringsAsFactors = FALSE
      )
    ),
    class = "canreg"
  )
  x <- structure(list(one_registry), class = "canregs")

  res <- cr_check(x, collapse = FALSE, quiet = TRUE)

  expect_s3_class(res, "checks")
  expect_type(res, "list")
})
