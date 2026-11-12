## code to prepare `IARC_BehaMorpRule` dataset goes here
morp_beha <- read_excel(
  rule_source_path("2023.ICD03toICD9CM-ICD10-ICD10CM.xlsx"),
  sheet = "2023.Histologies (ICD-O-3)"
) |>
  select(c(2, 5, 7))
colnames(morp_beha) <- c("morp", "info", "icd10")
morp_beha <- morp_beha |>
  filter(!is.na(morp)) |>
  distinct(morp, .keep_all = TRUE) |>
  pull(morp)

res <- data.frame(
  morp = substr(morp_beha, 1, 4),
  beha = substr(morp_beha, 6, 6)
) |>
  group_by(beha) |>
  reframe(morp = compress_morp(as.integer(morp), sep = ","))
encr_exprence <- c(
  "8000/2",
  "8011/2",
  "8051/2",
  "8053/2",
  "8053/3",
  "8071/2",
  "8072/2",
  "8075/2",
  "8078/2",
  "8083/2",
  "8084/2",
  "8100/3",
  "8103/3",
  "8121/2",
  "8123/2",
  "8124/2",
  "8131/2",
  "8143/2",
  "8144/2",
  "8160/2",
  "8211/2",
  "8213/2",
  "8220/2",
  "8221/2",
  "8248/3",
  "8250/2",
  "8251/2",
  "8252/2",
  "8253/2",
  "8255/2",
  "8260/2",
  "8262/2",
  "8271/3",
  "8310/2",
  "8380/2",
  "8382/2",
  "8384/2",
  "8400/2",
  "8401/2",
  "8402/2",
  "8403/2",
  "8409/2",
  "8410/2",
  "8440/1",
  "8441/2",
  "8443/3",
  "8444/3",
  "8480/2",
  "8481/2",
  "8482/2",
  "8490/2",
  "8502/2",
  "8507/3",
  "8508/2",
  "8510/2",
  "8540/2",
  "8542/2",
  "8543/2",
  "8550/2",
  "8560/2",
  "8570/2",
  "8573/2",
  "8590/3",
  "8721/2",
  "8722/2",
  "8723/2",
  "8740/2",
  "8743/2",
  "8744/2",
  "8745/2",
  "8746/2",
  "8770/2",
  "8771/2",
  "8772/2",
  "8825/3",
  "8832/1",
  "8940/2",
  "8941/2",
  "8983/3",
  "9013/3",
  "9061/2",
  "9084/2"
)
encr_exprence2 <- encr_exprence[encr_exprence %nin% morp_beha]

exprence <- data.frame(
  morp = substr(encr_exprence2, 1, 4),
  beha = substr(encr_exprence2, 6, 6)
) |>
  group_by(beha) |>
  reframe(morp = compress_morp(as.integer(morp), sep = ","))


morp_beha <- recode_morp(res$morp)
names(morp_beha) <- res$beha
IARC_BehaMorpRule <- morp_beha
rm(morp_beha, res)
