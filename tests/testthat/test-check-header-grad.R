test_that("check_header returns a check object with group field", {
  df <- data.frame(id = 1:3, age = c(30, 40, 50), extra_var = 1)

  res <- check_header(df, schema = "ncc")
  out <- print(res)

  expect_s3_class(res, "check")
  expect_true("group" %in% names(out))
})

test_that("check_grad_morp_beha validates lengths and class", {
  res <- check_grad_morp_beha(
    grad = c(1, 9),
    morp = c(8000, 8140),
    beha = c("3", "2"),
    quiet = TRUE
  )

  expect_s3_class(res, "check")
  expect_s3_class(res, "grad/morp/beha")
  expect_error(
    check_grad_morp_beha(grad = 1:2, morp = 8000, beha = 3, quiet = TRUE),
    "same length"
  )
})
