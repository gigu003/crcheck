# check_duplicate_person validates thresholds and columns

    Code
      check_duplicate_person(x, review_threshold = 0.95, match_threshold = 0.9)
    Condition
      Error:
      ! `review_threshold` and `match_threshold` must satisfy 0 <= review_threshold < match_threshold <= 1.

---

    Code
      check_duplicate_person(x, name = "patient_name")
    Condition
      Error:
      ! The following columns are missing from `x`: patient_name

---

    Code
      check_duplicate_person(data.frame(id = rep("11010519491231002X", 4), name = rep(
        "张三", 4), sex = rep(1, 4), birthda = rep("1949-12-31", 4)), max_pairs = 5)
    Condition
      Error:
      ! Blocking strategy `valid_id` would generate 6 pairs, exceeding `max_pairs` (5). Use stricter blocking or increase `max_pairs`.

---

    Code
      check_duplicate_person(data.frame(id = c("", ""), name = c("张三", "张三"), sex = c(
        1, 1), birthda = c("2000-01-01", "2000-01-01")))
    Condition
      Error:
      ! The unsupervised linkage model could not be fitted because the candidate pairs contain too little comparison variation. Supply more records or stronger identity fields.

