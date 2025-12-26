## code to prepare `ICCC3Rule` dataset goes here
library(rvest)
url <- "https://seer.cancer.gov/iccc/iccc3.html"
table <- html_table(read_html(url))[[1]]
iccc3_2005 <- table[-c(1,7,13,23,28,34,39,48,61,69,77,81),]
colnames(iccc3_2005) <- c("desc", "morp", "topo", "recode")
iccc3_2005 <- list(
  morp = lapply(strsplit(iccc3_2005$morp, ","), recode_morp),
  topo = lapply(strsplit(iccc3_2005$topo, ","), recode_topo),
  recode = as.list(iccc3_2005$recode)
)
v2005 <- list(
  main = iccc3_2005
)
ICCC3Rule <- list(
  v2005 = v2005
)

rm(url, table, iccc3_2005, v2005)
