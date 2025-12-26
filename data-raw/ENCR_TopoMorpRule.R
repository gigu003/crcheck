## code to prepare `ENCR_TopoMorpRule` dataset goes here
library(dplyr)
nallowed <- read.csv(paste0(addr,"MorphologyTopographyRule1.csv"),
                  sep = ";", header = FALSE)
colnames(nallowed) <- c("morp", "topo")
nallowed <- nallowed |> 
  group_by(topo) |> 
  arrange(morp) |> 
  reframe(morp = paste(morp, collapse = ",")) |> 
  group_by(morp) |> 
  reframe(topo = paste(topo, collapse = ","))
nallowed <- list(
  morp = lapply(strsplit(nallowed$morp, ","), as.integer),
  topo = lapply(strsplit(nallowed$topo, ","), recode_topo)
)

  
allowed <- read.csv(paste0(addr,"MorphologyTopographyRule2.csv"),
                  sep = ";", header = FALSE)
colnames(allowed) <- c("morp", "topo")
allowed <- allowed |> 
  group_by(topo) |> 
  arrange(morp) |> 
  reframe(morp = paste(morp, collapse = ",")) |> 
  group_by(morp) |> 
  reframe(topo = paste(topo, collapse = ","))
allowed <- list(
  morp = lapply(strsplit(allowed$morp, ","), as.integer),
  topo = lapply(strsplit(allowed$topo, ","), recode_topo)
)

ENCR_TopoMorpRule <- list(
  allowed = allowed,
  nallowed = nallowed
)

rm(allowed, nallowed)
