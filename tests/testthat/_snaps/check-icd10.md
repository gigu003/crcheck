# check_icd10 validates input lengths and quiet

    Code
      check_icd10(c("C50.9", "C18.0"), 8500, 3, "C50.9", quiet = TRUE)
    Condition
      Error:
      ! Arguments must have the same length: `topo` = 2, `morp` = 1, `beha` = 1, `icd10` = 1

---

    Code
      check_icd10("C50.9", 8500, 3, "C50.9", quiet = NA)
    Condition
      Error:
      ! `quiet` must be TRUE or FALSE.

