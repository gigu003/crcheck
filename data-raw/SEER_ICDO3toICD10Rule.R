## code to prepare `SEER_ICDO3toICD10Rule` dataset goes here
library(dplyr)
library(readxl)
addr1 <- "~/Documents/3_Resources/Website/db/data/ICDO3/"
file1 <- "2023.ICD03toICD9CM-ICD10-ICD10CM.xlsx"
group <- read_excel(paste0(addr1, file1), sheet = "2023.Histologies (ICD-O-3)") |>
  select(c(2,5,7)) 
colnames(group) <- c("morp", "info", "icd10")
group <- group |> filter(!is.na(morp)) |> 
  distinct(morp, .keep_all = TRUE) |> 
  mutate(
    cat = case_when(grepl("SPECIFIC SITES", toupper(info)) ~ 1,
                    grepl("ALL OTHER SITES", toupper(info)) ~ 2,
                    grepl("ALL SITES", toupper(info)) ~ 3)) |> 
  arrange(cat)

group1 <- group |> 
  filter(cat == 3) |> 
  group_by(icd10) |> 
  reframe(morp = paste(morp, collapse = ","))
group2 <- group |> 
  filter(!cat == 3) |> 
  mutate(
    loc = unlist(lapply(strsplit(icd10, "Col"), function(x) {
      gsub(" ", "",x[length(x)]) })),
    topo = unlist(lapply(strsplit(info, ","), function(x) {
      paste(x[-length(x)], collapse = ",")})),
    icd10 = unlist(lapply(strsplit(icd10, ","), function(x) {
      paste(x[-length(x)], collapse = ",")})) ) |>
  filter(!is.na(loc)) |> 
  select(morp, loc) |> 
  group_by(loc) |> 
  reframe(morp = paste(morp, collapse = ",")) 
  

group3 <- group |> 
  filter(!tolower(info) == "specific sites", !cat==3) |> 
  mutate(
    loc = unlist(lapply(strsplit(icd10, "Col"), function(x) {
      gsub(" ", "",x[length(x)]) })),
    topo = unlist(lapply(strsplit(info, ","), function(x) {
      paste(x[-length(x)], collapse = ",") })),
    icd10 = unlist(lapply(strsplit(icd10, ","), function(x) {
      paste(x[-length(x)], collapse = ",") }))) |> 
  select(morp, topo, icd10)

group33 <- lapply(group3$morp, function(x) {
  data.frame(
    sex = rep(1, 330),
    topo = gen_topo(0:809),
    morp = rep(substr(x,1,4), 330),
    beha = rep(substr(x,6,6), 330),
    grad = rep(9, 330))
})
group33 <- do.call(rbind, group33)
group33$registr <- row_number(group33) 
write.csv(group33, "icdo3.csv", row.names = FALSE)

rule1 <- list(
  morp = lapply(strsplit(group1$morp, ","), function(x) gsub(" ", "", x)),
  icd10 = as.list(group1$icd10)
  )
rule2 <- list(
  morp = lapply(strsplit(group2$morp, ","), function(x) gsub(" ", "", x)),
  loc = as.list(group2$loc)
  )
rule3 <- list(
  morp = lapply(strsplit(group3$morp, ","), function(x) gsub(" ", "", x)),
  topo = strsplit(group3$topo, ","),
  icd10 = strsplit(group3$icd10, ",")
  )

# topo to icd10
TopoICD10 <- read_excel(paste0(addr1, file1),
                        sheet = "Table B (ICD-10)",
                        range = "A5:K334",
                        col_names = LETTERS[1:11])
names<- TopoICD10$A
TopoICD10 <- as.matrix(TopoICD10[,-1])
rownames(TopoICD10) <- names
SEER_ICDO3toICD10Rule <- list(
  morp_map1 = rule1,
  morp_map2 = rule2,
  morp_map3 = rule3,
  topo_map = TopoICD10
)

rm(TopoICD10, rule1, rule2, rule3, group1, group2, group3, group)

