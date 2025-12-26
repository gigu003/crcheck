## code to prepare `IARC_BehaMorpRule` dataset goes here
library(dplyr)
library(readxl)
addr1 <- "~/Documents/3_Resources/Website/db/data/ICDO3/"
file1 <- "2023.ICD03toICD9CM-ICD10-ICD10CM.xlsx"
morp_beha <- read_excel(paste0(addr1, file1), sheet = "2023.Histologies (ICD-O-3)") |>
  select(c(2,5,7)) 
colnames(morp_beha) <- c("morp", "info", "icd10")
morp_beha <- morp_beha |> filter(!is.na(morp)) |> 
  distinct(morp, .keep_all = TRUE) |> 
  pull(morp)
IARC_BehaMorpRule <- list(morp_beha = morp_beha)
rm(morp_beha)