library(canregR)
library(dplyr)
data <- read_sqlite(show_registry(1:6), c(2023, 2025))
crquality <- crcheck::cr_check(data, collapse = TRUE)
as_tibble(crquality) |> count(type)
cases <- read_case("%")
cr_example <- cases |>
  filter(registr %in% crquality$id) |>
  select(
    registr,
    sex,
    birthda,
    inciden,
    topo,
    morp,
    beha,
    grad,
    basi,
    icd10,
    mult,
    status,
    caus,
    deathda,
    lastcontact
  )
use_data(cr_example, overwrite = TRUE)
