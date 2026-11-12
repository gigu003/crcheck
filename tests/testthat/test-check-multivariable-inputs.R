test_that("multivariable check functions validate equal lengths", {
  expect_error(
    check_topo_morp(topo = c("C15.1", "C34.9"), morp = 8140, quiet = TRUE),
    "same length"
  )

  expect_error(
    check_age_topo_morp(
      age = c(30, 40),
      topo = "C61.9",
      morp = 8140,
      quiet = TRUE
    ),
    "same length"
  )

  expect_error(
    check_sex_topo_morp(
      sex = c(1, 2),
      topo = "C61.9",
      morp = 8140,
      quiet = TRUE
    ),
    "same length"
  )

  expect_error(
    check_beha_topo(beha = c(2, 3), topo = "C40.9", quiet = TRUE),
    "same length"
  )

  expect_error(
    check_basi_morp(
      basi = c(1, 2),
      morp = 8140,
      topo = "C15.2",
      beha = 3,
      quiet = TRUE
    ),
    "same length"
  )

  expect_error(
    check_grad_topo_morp_beha(
      grad = c(1, 2),
      topo = "C15.2",
      morp = 8140,
      beha = 3,
      quiet = TRUE
    ),
    "same length"
  )
})

test_that("multivariable check functions validate quiet flag", {
  expect_error(
    check_topo_morp(topo = "C15.1", morp = 8140, quiet = NA),
    "quiet"
  )

  expect_error(
    check_age_topo_morp(age = 30, topo = "C61.9", morp = 8140, quiet = "yes"),
    "quiet"
  )

  expect_error(
    check_sex_topo_morp(
      sex = 1,
      topo = "C61.9",
      morp = 8140,
      quiet = c(TRUE, FALSE)
    ),
    "quiet"
  )

  expect_error(
    check_beha_topo(beha = 2, topo = "C40.9", quiet = NA),
    "quiet"
  )

  expect_error(
    check_basi_morp(
      basi = 1,
      morp = 8140,
      topo = "C15.2",
      beha = 3,
      quiet = NA
    ),
    "quiet"
  )

  expect_error(
    check_grad_topo_morp_beha(
      grad = 1,
      topo = "C15.2",
      morp = 8140,
      beha = 3,
      quiet = NA
    ),
    "quiet"
  )
})

test_that("multivariable check functions accept character-coded numeric inputs", {
  res_topo_morp <- check_topo_morp(
    topo = c("C15.1", "C34.9"),
    morp = c("8140", "8600"),
    quiet = TRUE
  )

  res_age_topo_morp <- check_age_topo_morp(
    age = "30",
    topo = "C61.9",
    morp = "8140",
    quiet = TRUE
  )

  res_sex_topo_morp <- check_sex_topo_morp(
    sex = c("1", "2"),
    topo = c("C51.0", "C60.0"),
    morp = c("8950", "9300"),
    quiet = TRUE
  )

  res_beha_topo <- check_beha_topo(
    beha = "2",
    topo = "C40.9",
    quiet = TRUE
  )

  res_basi_morp <- check_basi_morp(
    basi = "1",
    morp = "8140",
    topo = "C15.2",
    beha = "3",
    quiet = TRUE
  )

  res_grad_topo_morp_beha <- check_grad_topo_morp_beha(
    grad = "1",
    topo = "C15.2",
    morp = "8140",
    beha = "3",
    quiet = TRUE
  )

  expect_s3_class(res_topo_morp, "check")
  expect_s3_class(res_age_topo_morp, "check")
  expect_s3_class(res_sex_topo_morp, "check")
  expect_s3_class(res_beha_topo, "check")
  expect_s3_class(res_basi_morp, "check")
  expect_s3_class(res_grad_topo_morp_beha, "check")
})
