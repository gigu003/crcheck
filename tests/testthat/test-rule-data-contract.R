test_that("embedded rule objects retain their build contract", {
  contract <- utils::read.csv(
    testthat::test_path("fixtures", "rule-object-contract.csv"),
    na.strings = "NA",
    stringsAsFactors = FALSE
  )
  namespace <- asNamespace("crcheck")

  actual <- lapply(contract$object, function(name) {
    object <- get(name, envir = namespace, inherits = FALSE)
    data.frame(
      object = name,
      class = class(object)[[1]],
      length = length(object),
      nrow = if (is.data.frame(object)) nrow(object) else NA_integer_,
      stringsAsFactors = FALSE
    )
  })
  actual <- do.call(rbind, actual)
  rownames(actual) <- NULL

  expect_identical(actual, contract)
})
