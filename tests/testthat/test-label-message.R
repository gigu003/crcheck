test_that("label_message adds Chinese labels by default", {
  x <- data.frame(
    id = c("case-a", "case-b"),
    item = c("sex", "Sex/Topo"),
    value = c("9", "1/C53.0"),
    type = c("E-RANGE", "E-SETO"),
    group = c("G-02", "G-01"),
    stringsAsFactors = FALSE
  )

  res <- label_message(x)

  expect_equal(nrow(res), nrow(x))
  expect_equal(names(res), c(names(x), "label_cn"))
  expect_equal(res$label_cn[1], "值超出允许范围")
  expect_equal(res$label_cn[2], "男性肿瘤不能发生于女性生殖器官部位")
})

test_that("label_message can add English labels", {
  x <- data.frame(
    type = c("E-RANGE", "E-SETO"),
    group = c("G-02", "G-01"),
    stringsAsFactors = FALSE
  )

  res <- label_message(x, lang = "en")

  expect_equal(names(res), c(names(x), "label_en"))
  expect_equal(res$label_en[1], "Value is outside the permitted range")
  expect_equal(
    res$label_en[2],
    "A male patient cannot have a tumour at a female genital organ site"
  )
})

test_that("label_message labels inconsistent ICD-10 results", {
  x <- data.frame(
    type = "E-ICD10",
    group = "G-01",
    stringsAsFactors = FALSE
  )

  res_cn <- label_message(x)
  res_en <- label_message(x, lang = "en")

  expect_equal(
    res_cn$label_cn,
    "ICD-10编码与肿瘤部位、形态学和行为学编码转换结果不一致"
  )
  expect_equal(
    res_en$label_en,
    paste(
      "The ICD-10 code is inconsistent with the code converted from",
      "topography, morphology and behaviour"
    )
  )
})

test_that("label_message supports a custom label column name", {
  x <- data.frame(
    type = "E-RANGE",
    group = "G-02",
    stringsAsFactors = FALSE
  )

  res <- label_message(x, label_col = "message")

  expect_equal(names(res), c(names(x), "message"))
  expect_equal(res$message, "值超出允许范围")
})

test_that("label_message preserves unmatched messages as NA", {
  x <- data.frame(
    type = "W-NOTFOUND",
    group = "G-99",
    stringsAsFactors = FALSE
  )

  res <- label_message(x)

  expect_true(is.na(res$label_cn))
})

test_that("label_message handles empty check result data frames", {
  x <- data.frame(
    id = character(0),
    item = character(0),
    value = character(0),
    type = character(0),
    group = character(0),
    stringsAsFactors = FALSE
  )

  res <- label_message(x)

  expect_equal(nrow(res), 0L)
  expect_equal(names(res), c(names(x), "label_cn"))
})

test_that("label_message accepts check objects", {
  x <- check_single_item("9", var_name = "sex", quiet = TRUE)

  res <- label_message(x, lang = "en")

  expect_s3_class(res, "data.frame")
  expect_equal(res$label_en, "Value is outside the permitted range")
})

test_that("label_message keeps check data frames as data frames", {
  x <- data.frame(
    type = "E-SETO",
    group = "G-01",
    stringsAsFactors = FALSE
  )
  class(x) <- c("check", "data.frame")

  res <- label_message(x)

  expect_s3_class(res, "data.frame")
  expect_equal(res$label_cn, "男性肿瘤不能发生于女性生殖器官部位")
})

test_that("label_message validates inputs", {
  expect_error(label_message("not a data frame"), "data frame")

  expect_error(
    label_message(data.frame(type = "E-RANGE")),
    "group"
  )

  expect_error(
    label_message(data.frame(type = "E-RANGE", group = "G-XX"), lang = "fr"),
    "arg"
  )

  expect_error(
    label_message(
      data.frame(type = "E-RANGE", group = "G-XX"),
      label_col = ""
    ),
    "label_col"
  )
})
