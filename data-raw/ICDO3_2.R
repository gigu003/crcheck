## code to prepare `ICDO3_2` dataset goes here
nallowed <- read.csv(paste0(addr,"MorphologyTopographyRule1.csv"),
                     sep = ";", header = FALSE)
colnames(nallowed) <- c("morp", "topo")
nallowed <- nallowed |> 
  group_by(morp) |> 
  arrange(topo) |> 
  reframe(topo = paste(topo, collapse = ","))

allowed <- read.csv(paste0(addr,"MorphologyTopographyRule2.csv"),
                    sep = ";", header = FALSE)
colnames(allowed) <- c("morp", "topo")
allowed <- allowed |> 
  group_by(morp) |> 
  arrange(topo) |> 
  reframe(topo = paste(topo, collapse = ","))

morp32en <- read.csv("~/Documents/3_Resources/Website/db/data/morp.csv")
morp32cn <- read.csv("~/Documents/3_Resources/Website/db/data/morp2.csv")
morp32 <- morp32en |> 
  mutate(morp = paste0("M", substr(morp, 1, 6))) |>
  left_join(morp32cn, by = c("morp")) |> 
  select(-mark, -behaviour) |> 
  mutate(des = gsub("SPECIFIC SITES", "1", toupper(o3site)),
         morp2 = as.integer(substr(morp, 2, 5))) |> 
  left_join(allowed, by = c("morp2" = "morp")) |> 
  rename(allowed = topo) |> 
  left_join(nallowed, by = c("morp2" = "morp")) |> 
  rename(nallowed = topo) |> 
  select(morp, morp_desen, morp_descn, o3site, allowed, nallowed, comments)

rm(allowed, nallowed, morp32en, morp32cn)



