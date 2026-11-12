test_that("check_basi_morp example returns a printable check object", {
  basi <- c(1, 1, 1, 1, 5, 7, 2)
  beha <- c(3, 3, 3, 3, 3, 3, 3)
  morp <- c(8000, 8720, 9120, 9510, 8023, 8001, 9180)
  topo <- c("C15.2", "C15.2", "C44.0", "C69.1", "C42.1", "C15.2", "C15.2")

  res <- check_basi_morp(basi, morp, topo, beha, quiet = TRUE)
  out <- print(res)

  expect_s3_class(res, "check")
  expect_s3_class(out, "data.frame")
  expect_true(all(c("id", "item", "value", "type", "group") %in% names(out)))
})

test_that("check_basi_morp prints basis, morphology, topography, and behaviour", {
  res <- check_basi_morp(
    basi = 1,
    morp = 9120,
    topo = "C44.0",
    beha = 3,
    quiet = TRUE
  )
  out <- print(res)

  expect_true(nrow(out) > 0L)
  expect_match(out$value[1], "^1/9120/C44\\.0/3$")
})

test_that("check_basi_morp allows missing optional topography argument", {
  res <- check_basi_morp(
    basi = c(1, 2),
    morp = c(9120, 9180),
    beha = c(3, 3),
    quiet = TRUE
  )

  expect_s3_class(res, "check")
  expect_equal(length(res$check), length(res$data))
})
