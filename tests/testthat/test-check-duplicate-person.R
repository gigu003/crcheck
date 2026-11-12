test_that("check_duplicate_person groups two or more records", {
  x <- data.frame(
    record = paste0("R", 1:5),
    id = c(rep("11010519491231002X", 4), ""),
    name = c(rep("张三", 4), "李四"),
    sex = c(rep(1, 4), 2),
    birthda = c(rep("1949-12-31", 4), "1980-01-01"),
    stringsAsFactors = FALSE
  )

  res <- check_duplicate_person(x, record_id = "record", quiet = TRUE)

  expect_s3_class(res, "duplicate_person_result")
  expect_named(res, c("groups", "pairs", "model"))
  expect_equal(nrow(res$groups), 4L)
  expect_equal(unique(res$groups$group_size), 4L)
  expect_equal(unique(res$groups$minimum_pair_probability), 1)
  expect_equal(nrow(res$pairs), 6L)
  expect_equal(unique(res$pairs$probability_source), "verified_id")
})

test_that("different valid identity numbers prevent grouping", {
  x <- data.frame(
    id = c("11010519491231002X", "110105194912310038"),
    name = c("张三", "张三"),
    sex = c(1, 1),
    birthda = c("1949-12-31", "1949-12-31"),
    stringsAsFactors = FALSE
  )

  res <- check_duplicate_person(
    x,
    keep_pairs = c("same_person", "review", "different_person"),
    quiet = TRUE
  )

  expect_equal(nrow(res$groups), 0L)
  expect_equal(res$pairs$duplicate_probability, 0)
  expect_equal(res$pairs$decision, "different_person")
  expect_equal(res$pairs$probability_source, "verified_id")
})

test_that("strict grouping does not use transitive chaining", {
  pairs <- data.frame(
    row_1 = c(1L, 2L, 1L),
    row_2 = c(2L, 3L, 3L),
    duplicate_probability = c(0.99, 0.98, 0.80),
    decision = c("same_person", "same_person", "review"),
    stringsAsFactors = FALSE
  )

  grouped <- duplicate_complete_link(pairs, n_records = 3L, threshold = 0.95)

  expect_equal(grouped$membership, c(1L, 1L, 0L))
  expect_equal(grouped$status, c("in_group", "grouping_conflict", "review"))
})

test_that("strict grouping accepts a complete high-probability group", {
  pairs <- data.frame(
    row_1 = c(1L, 1L, 2L),
    row_2 = c(2L, 3L, 3L),
    duplicate_probability = c(0.99, 0.97, 0.98),
    decision = rep("same_person", 3),
    stringsAsFactors = FALSE
  )

  grouped <- duplicate_complete_link(pairs, n_records = 3L, threshold = 0.95)

  expect_equal(grouped$membership, c(1L, 1L, 1L))
  expect_equal(grouped$status, rep("in_group", 3))
})

test_that("unlabelled linkage uses Chinese text and optional address evidence", {
  x <- data.frame(
    id = "",
    name = c(
      " 张·一 ",
      "张一",
      "张二",
      "张三",
      "张四",
      "张五",
      "张六",
      "张七",
      "李一",
      "李二",
      "李三",
      "李四"
    ),
    sex = c(1, 1, 1, 1, 2, 2, 1, 2, 1, 2, 1, 2),
    birthda = c(
      rep("1980-01-01", 6),
      rep("1981-02-02", 2),
      rep("1980-01-01", 2),
      rep("1981-02-02", 2)
    ),
    address = c(
      "河南郑州一号",
      "河南郑州一号",
      "河南郑州二号",
      "北京三号",
      "河南四号",
      "北京五号",
      "郑州六号",
      "郑州七号",
      "开封一号",
      "开封二号",
      "洛阳三号",
      "洛阳四号"
    ),
    stringsAsFactors = FALSE
  )

  res <- check_duplicate_person(x, address = "address", quiet = TRUE)

  expect_equal(nrow(res$groups), 2L)
  expect_equal(res$model$method, "fellegi_sunter_em")
  expect_equal(res$model$converged, TRUE)
  expect_equal(res$pairs$name_similarity, 1)
  expect_equal(res$pairs$address_similarity, 1)
})

test_that("default output omits singletons and low-probability pairs", {
  x <- data.frame(
    id = c("11010519491231002X", "110105194912310038", ""),
    name = c("张三", "张三", "李四"),
    sex = c(1, 1, 2),
    birthda = c("1949-12-31", "1949-12-31", "1980-01-01")
  )

  res <- check_duplicate_person(x, quiet = TRUE)

  expect_equal(nrow(res$groups), 0L)
  expect_equal(nrow(res$pairs), 0L)
})

test_that("empty input returns typed empty results", {
  x <- data.frame(
    id = character(),
    name = character(),
    sex = integer(),
    birthda = character()
  )

  res <- check_duplicate_person(x, quiet = TRUE)

  expect_equal(nrow(res$groups), 0L)
  expect_equal(nrow(res$pairs), 0L)
  expect_equal(res$model$method, "not_fitted")
})

test_that("parallel and single-worker comparison agree", {
  skip_on_cran()
  x <- data.frame(
    id = rep("11010519491231002X", 46),
    name = rep("张三", 46),
    sex = rep(1, 46),
    birthda = rep("1949-12-31", 46)
  )

  one <- check_duplicate_person(x, n_cores = 1, quiet = TRUE)
  two <- check_duplicate_person(x, n_cores = 2, quiet = TRUE)

  expect_equal(two$groups, one$groups)
  expect_equal(two$pairs, one$pairs)
})

test_that("check_duplicate_person validates thresholds and columns", {
  x <- data.frame(id = "", name = "张三", sex = 1, birthda = "2000-01-01")

  expect_snapshot(error = TRUE, {
    check_duplicate_person(x, review_threshold = 0.95, match_threshold = 0.9)
  })
  expect_snapshot(error = TRUE, {
    check_duplicate_person(x, name = "patient_name")
  })
  expect_snapshot(error = TRUE, {
    check_duplicate_person(
      data.frame(
        id = rep("11010519491231002X", 4),
        name = rep("张三", 4),
        sex = rep(1, 4),
        birthda = rep("1949-12-31", 4)
      ),
      max_pairs = 5
    )
  })
  expect_snapshot(error = TRUE, {
    check_duplicate_person(data.frame(
      id = c("", ""),
      name = c("张三", "张三"),
      sex = c(1, 1),
      birthda = c("2000-01-01", "2000-01-01")
    ))
  })
})
