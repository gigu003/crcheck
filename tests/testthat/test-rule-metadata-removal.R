test_that("rule provenance API and internal manifest are absent", {
  expect_identical("rule_info" %in% getNamespaceExports("crcheck"), FALSE)
  expect_identical(
    exists("RuleManifest", envir = asNamespace("crcheck"), inherits = FALSE),
    FALSE
  )
  expect_identical("source" %in% names(IARC_MultiplePrimaryRule), FALSE)
})

test_that("deterministic checks do not retain rule metadata", {
  checks <- list(
    check_single_item(1, var_name = "sex", quiet = TRUE),
    check_age_birth_inci(40, "1980-01-01", "2020-01-01", quiet = TRUE),
    check_age_topo_morp(40, "C18.0", 8140, quiet = TRUE),
    check_topo_morp("C18.0", 8140, quiet = TRUE),
    check_sex_topo_morp(1, "C18.0", 8140, quiet = TRUE),
    check_beha_topo(3, "C18.0", quiet = TRUE),
    check_basi_morp(7, 8140, "C18.0", 3, quiet = TRUE),
    check_grad_morp_beha(1, 8140, 3, quiet = TRUE),
    check_grad_topo_morp_beha(1, "C18.0", 8140, 3, quiet = TRUE),
    check_icd10("C18.0", 8140, 3, "C18.9", quiet = TRUE),
    check_birth_inci_last(
      "1980-01-01",
      "2020-01-01",
      "2021-01-01",
      quiet = TRUE
    ),
    check_followup(
      7,
      1,
      "2020-01-01",
      NA_character_,
      "2021-01-01",
      quiet = TRUE
    ),
    check_header(data.frame(id = "case-1"))
  )

  expect_identical(
    vapply(checks, \(x) "check_version" %in% names(x), logical(1)),
    rep(FALSE, length(checks))
  )
  expect_identical(
    vapply(checks, \(x) is.null(attr(x, "rule_id")), logical(1)),
    rep(TRUE, length(checks))
  )
  expect_identical(
    vapply(checks, \(x) is.null(attr(x, "rule_info")), logical(1)),
    rep(TRUE, length(checks))
  )
  expect_identical(checks[[13L]]$check_standard, "ncc")
})

test_that("summaries and comprehensive results omit rule metadata", {
  check <- check_single_item(9, var_name = "sex", quiet = TRUE)
  summary_text <- capture.output(summary(check))
  expect_identical(any(grepl("Standard Version", summary_text)), FALSE)

  primary <- check_multiple_primary(
    data.frame(
      id = c("P1", "P1"),
      topo = c("C18.0", "C34.9"),
      morp = c(8140, 8140)
    ),
    quiet = TRUE
  )
  primary_text <- capture.output(crcheck:::mp_summary(primary))
  expect_null(attr(primary, "check_version"))
  expect_null(attr(primary, "rule_id"))
  expect_null(attr(primary, "rule_info"))
  expect_identical(any(grepl("Standard Version", primary_text)), FALSE)

  result <- cr_check(data.frame(sex = 1), quiet = TRUE)
  expect_identical(names(result), c("id", "item", "value", "type", "group"))
  expect_null(attr(result, "rule_id"))
  expect_null(attr(result, "rule_info"))

  one <- structure(
    list(FBcases = data.frame(sex = 1), areacode = "A"),
    class = "canreg"
  )
  many <- structure(list(one, one), class = "canregs")
  outputs <- list(
    cr_check(one, quiet = TRUE),
    cr_check(many, collapse = TRUE, quiet = TRUE),
    cr_check(many, collapse = FALSE, quiet = TRUE)
  )
  expect_identical(
    vapply(outputs, \(x) is.null(attr(x, "rule_id")), logical(1)),
    rep(TRUE, length(outputs))
  )
  expect_identical(
    vapply(outputs, \(x) is.null(attr(x, "rule_info")), logical(1)),
    rep(TRUE, length(outputs))
  )
})
