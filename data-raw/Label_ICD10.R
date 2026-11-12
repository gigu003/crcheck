special_c <- c(
  1,
  7,
  12,
  19,
  20,
  23,
  33,
  37,
  52,
  55,
  56,
  58,
  61,
  64,
  65,
  66,
  73,
  80,
  97
)
special_c <- sprintf("C%02d", special_c)
special_d <- c(24, 27, 34, 45)
special_d <- sprintf("D%02d", special_d)
clean_icd <- function(x) {
  x <- icd10$des
  res <- gsub("[^CD0-9.\\-]", "", x)
  res <- ifelse(
    grepl("^[CD][0-9]{2}-[CD][0-9]{2}$", res),
    res,
    gsub("-", "", res)
  )
  res <- ifelse(nchar(res) > 5 & !grepl("-", res), substr(res, 1, 5), res)
  res
}
icd10 <- read_excel(rule_source_path("icd10_label.xlsx"))
icd101 <- icd10 |>
  mutate(
    icd10 = clean_icd(des),
    icd10 = ifelse(
      substr(icd10, 1, 3) %in% c(special_c, special_d),
      substr(icd10, 1, 3),
      icd10
    ),
    des_cn = gsub("[xXCD0-9.\\-]", "", des),
    des_cn = stringr::str_trim(des_cn),
    des_cn = gsub("[()]", "", des_cn)
  ) |>
  group_by(icd10) |>
  reframe(des_cn = paste(unique(des_cn), collapse = "aabb"))

Label_ICD10_cn <- strsplit(icd101$des_cn, split = "aabb")
names(Label_ICD10_cn) <- icd101$icd10
rm(icd10, icd101, special_c, special_d, clean_icd)


## 处理topo标签
topo <- read_excel(
  rule_source_path("ICDO3.xlsx"),
  sheet = "ICD-O-T"
)
names(topo) <- c("mark", "topo", "descn")
Label_topo_cn <- strsplit(topo$descn, split = "；")
names(Label_topo_cn) <- topo$topo
rm(topo)


## 处理 Morp 标签
morp <- read_excel(
  rule_source_path("ICDO3.xlsx"),
  sheet = "ICD-O-M"
)
names(morp) <- c("mark", "morp", "descn", "transcode")
Label_morp_cn <- strsplit(morp$descn, split = "；")
names(Label_morp_cn) <- morp$morp
rm(morp)


## 查找解剖学部位相关的形态学编码
morp <- read_excel(
  rule_source_path("2023.ICD03toICD9CM-ICD10-ICD10CM.xlsx"),
  sheet = "2023.Histologies (ICD-O-3)"
)
names(morp) <- c(
  "mark",
  "morp",
  "desen",
  "beha",
  "topo",
  "icd9cm",
  "icd10",
  "icd10cm",
  "comments"
)
morp2 <- morp |>
  filter(!is.na(morp), grepl("[C][0-9]{2,3}_?", desen))
res3 <- str_extract_all(morp2$desen, "C[0-9]{3}")
tmp <- str_remove_all(morp2$desen, "C[0-9]{3}")
res2 <- str_extract_all(tmp, "[C][0-9]{2,3}")
res <- Map(c, res3, res2)
names(res) <- morp2$morp

data <- data.frame(
  morp = substr(names(res), 1, 4),
  beha = substr(names(res), 6, 6),
  topo = sapply(res, function(x) paste(unique(x), collapse = ", "))
) |>
  group_by(morp) |>
  reframe(
    beha = paste(unique(beha), collapse = ", "),
    topo = paste(unique(topo), collapse = ", ")
  ) |>
  group_by(topo) |>
  reframe(morp = paste(unique(morp), collapse = ", "))
morp_suggest_topo <- list(
  morp = recode_morp(data$morp),
  topo = recode_topo(data$topo)
)
rm(morp, morp2, res, res2, res3, tmp, data)
