test_that("ID last character errors keep original input value", {
  id <- "410105199001011231"

  res <- check_single_item(id, var_name = "id", quiet = TRUE)
  out <- print(res)
  last_char <- out[out$type == "E-ID-LastChar", , drop = FALSE]

  expect_equal(nrow(last_char), 1L)
  expect_equal(last_char$value, id)
  expect_false(is.na(last_char$value))
})

test_that("ID last character helper returns empty integer vector for unchecked IDs", {
  expect_identical(id_last_unvalid_idx(NA_character_), integer(0L))
  expect_identical(id_last_unvalid_idx("not-an-id"), integer(0L))
})
