## prepare the data for Rules in DiagnosisMorphologyRule
## Rule NO. 1
# Rule basis equals 0
basi0 <- read.csv(
  file.path(encr2014, "DiagnosisMorphologyRuleBoD0.csv"),
  sep = ";",
  header = FALSE
)
colnames(basi0) <- c("morp", "beha", "topo")
basi0 <- list(
  morp = lapply(basi0$morp, recode_morp),
  beha = as.list(basi0$beha),
  topo = lapply(strsplit(basi0$topo, ","), recode_topo)
)
# Rule basis equals 1
basi1 <- read.csv(
  file.path(encr2014, "DiagnosisMorphologyRuleBoD1.csv"),
  sep = ";",
  header = FALSE
)
colnames(basi1) <- c("morp", "beha", "topo")
basi1 <- list(
  morp = recode_morp(basi1$morp),
  beha = as.list(basi1$beha),
  topo = recode_topo(basi1$topo)
)
# Rule basis equals 2
basi2 <- read.csv(
  file.path(encr2014, "DiagnosisMorphologyRuleBoD2.csv"),
  sep = ";",
  header = FALSE
)
colnames(basi2) <- c("morp", "beha", "topo")
basi2 <- list(
  morp = recode_morp(basi2$morp),
  beha = lapply(basi2$beha, function(x) {
    if (x < 10) {
      x
    } else if (x < 100) {
      as.integer(c(floor(x / 10), x %% 10))
    }
  }),
  topo = recode_topo(basi2$topo)
)
# Rule basis equals 4
basi4 <- read.csv(
  file.path(encr2014, "DiagnosisMorphologyRuleBoD4.csv"),
  sep = ";",
  header = FALSE
)
colnames(basi4) <- c("morp", "beha", "topo")
basi4 <- basi4 |>
  group_by(morp, beha) |>
  reframe(topo = paste(topo, collapse = ","))
basi4 <- list(
  morp = recode_morp(basi4$morp),
  beha = lapply(basi4$beha, function(x) {
    if (x < 10) {
      x
    } else if (x < 100) {
      as.integer(c(floor(x / 10), x %% 10))
    }
  }),
  topo = recode_topo(basi4$topo)
)
# Rule basis equals 5
basi5 <- read.csv(
  file.path(encr2014, "DiagnosisMorphologyRuleBoD5.csv"),
  sep = ";",
  header = FALSE
)
colnames(basi5) <- c("morp", "beha")
basi5 <- list(
  morp = lapply(strsplit(basi5$morp, ","), recode_morp),
  beha = as.list(basi5$beha)
)
# Rule basis equals 6
basi6 <- read.csv(
  file.path(encr2014, "DiagnosisMorphologyRuleBoD6.csv"),
  sep = ";",
  header = FALSE
)
colnames(basi6) <- c("morp", "beha")
basi6 <- list(
  morp = recode_morp(basi6$morp),
  beha = as.list(basi6$beha)
)
# Rule basis equals 7
basi7 <- read.csv(
  file.path(encr2014, "DiagnosisMorphologyRuleBoD7.csv"),
  sep = ";",
  header = FALSE
)
colnames(basi7) <- c("morp", "beha")
basi7 <- list(
  morp = recode_morp(basi7$morp),
  beha = as.list(basi7$beha)
)
# Rule basis equals 8
basi8 <- read.csv(
  file.path(encr2014, "DiagnosisMorphologyRuleBoD8.csv"),
  sep = ";",
  header = FALSE
)
basi8 <- recode_morp(basi8$V1)

ENCR_BasiMorpRule <- list(
  basi0 = basi0,
  basi1 = basi1,
  basi2 = basi2,
  basi4 = basi4,
  basi5 = basi5,
  basi6 = basi6,
  basi7 = basi7,
  basi8 = basi8
)
rm(basi0, basi1, basi2, basi4, basi5, basi6, basi7, basi8)
