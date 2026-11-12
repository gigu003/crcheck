test_that("0.1.0 exports match the public API contract", {
  contract_path <- testthat::test_path("fixtures", "exports-0.1.0.txt")
  expected <- readLines(contract_path, warn = FALSE)

  expect_identical(sort(getNamespaceExports("crcheck")), expected)
})

test_that("legacy exports remain removed", {
  legacy_exports <- c(
    "ICDO3toICD10",
    "check_age",
    "check_basi",
    "check_beha",
    "check_grad",
    "check_id",
    "check_morp",
    "check_morp_beha",
    "check_morp_grad",
    "check_occu",
    "check_sex",
    "check_sex_morp",
    "check_sex_topo",
    "check_topo",
    "check_topo_morp_beha_grad",
    "check_trib",
    "show_age_topo_morp",
    "show_grad_morp",
    "show_morp_beha",
    "show_sex_morp",
    "show_sex_topo",
    "show_topo_morp"
  )

  expect_identical(
    intersect(legacy_exports, getNamespaceExports("crcheck")),
    character()
  )
})

test_that("installed metadata identifies 0.1.0.9000", {
  expect_identical(as.character(utils::packageVersion("crcheck")), "0.1.0.9000")
})

test_that("formal tests exclude private rule sources", {
  test_root <- testthat::test_path()
  test_files <- list.files(
    test_root,
    pattern = "^test-.*[.]R$",
    recursive = FALSE,
    full.names = TRUE
  )
  private_path <- paste(c("data-raw", "private"), collapse = "/")
  test_source <- unlist(
    lapply(test_files, readLines, warn = FALSE),
    use.names = FALSE
  )

  expect_identical(any(grepl(private_path, test_source, fixed = TRUE)), FALSE)
})
