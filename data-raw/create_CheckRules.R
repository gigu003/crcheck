library(dplyr)
library(readxl)
library(stringr)

rule_source_dir <- file.path("data-raw", "private")
rule_source_path <- function(...) file.path(rule_source_dir, ...)
encr2014 <- rule_source_path("encr2014")

source("R/zzz.R")
source("R/generate.R")
source("R/recode.R")
source("R/show_rules.R")

source("data-raw/ValueRange.R")
source("data-raw/ENCR_GradMorpRule.R")
source("data-raw/ENCR_BasiMorpRule.R")
source("data-raw/ENCR_TopoMorpBehaGradRule.R")
source("data-raw/IARC_AgeTopoMorpRule.R")
source("data-raw/IARC_GradMorpRule.R")
source("data-raw/IARC_TopoMorpRule.R")
source("data-raw/IARC_BehaMorpRule.R")
source("data-raw/IARC_BasiMorpRule.R")
source("data-raw/ICDO3_2.R")
source("data-raw/ICDO3toICD10Rule.R")
source("data-raw/SchemaHeader.R")
source("data-raw/Req_Vars_Func.R")
source("data-raw/IARC_FollowUpRule.R")
source("data-raw/IARC_MultiplePrimaryRule.R")
source("data-raw/Label_ICD10.R")
source("data-raw/OutputMessage.R")

usethis::use_data(
  ENCR_BasiMorpRule,
  ENCR_GradMorpRule,
  ENCR_TopoMorpBehaGradRule,
  IARC_AgeTopoMorpRule,
  IARC_GradMorpRule,
  IARC_TopoMorpRule,
  IARC_BehaMorpRule,
  IARC_BasiMorpRule,
  IARC_FollowUpRule,
  IARC_MultiplePrimaryRule,
  SchemaHeader,
  ValueRange,
  Req_Vars_Func,
  dict_morp,
  dict_topo,
  dict_icd10,
  dict_output_message,
  Label_ICD10_cn,
  Label_topo_cn,
  Label_morp_cn,
  morp_suggest_topo,
  ICDO3toICD10Rule,
  internal = TRUE,
  overwrite = TRUE,
  compress = "xz",
  version = 3
)
