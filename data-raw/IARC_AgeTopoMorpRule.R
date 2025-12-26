# Generate IARCAgeTopoMorpRule
library(readxl)
library(here)
data <- read_excel(here::here("data-raw/data", "age_morp_topo.xlsx"))
age <- lapply(data$age, function(x){
  sign <- gsub("[0-9]", "", x)
  if (sign == "-"){
    before <- sub("-.*", "", x)
    after <- sub(".*-", "", x)
    before:after
  } else if (sign == ">"){
    (as.integer(gsub("[^0-9]", "", x))+1):110
  } else if (sign == "<"){
    0:(as.integer(gsub("[^0-9]", "", x))-1)
  }
})

morp <- lapply(strsplit(data$morp, ","), recode_morp)
morp <- lapply(morp, function(x){
  if (length(x) == 0) {
    gen_morp(8000:9993)
  } else {x} 
})

topo <- lapply(strsplit(data$topo, ","), recode_topo)
topo <- lapply(topo, function(x){
  if (length(x) == 0) {
    gen_topo(0:809)
  } else {x} 
})

IARC_AgeTopoMorpRule <- list(un_age = age, topo = topo, morp = morp)
rm(data, age, topo, morp)
