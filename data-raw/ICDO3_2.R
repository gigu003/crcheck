## code to prepare `ICDO3_2` dataset goes here
encr <- rule_source_path("encr2014")
nallowed <- read.csv(
  file.path(encr, "MorphologyTopographyRule1.csv"),
  sep = ";",
  header = FALSE
)
colnames(nallowed) <- c("morp", "topo")
nallowed <- nallowed |>
  group_by(morp) |>
  arrange(topo) |>
  reframe(topo = paste(topo, collapse = ","))

allowed <- read.csv(
  file.path(encr, "MorphologyTopographyRule2.csv"),
  sep = ";",
  header = FALSE
)
colnames(allowed) <- c("morp", "topo")
allowed <- allowed |>
  group_by(morp) |>
  arrange(topo) |>
  reframe(topo = paste(topo, collapse = ","))

morp32en <- read.csv(rule_source_path("morp.csv"))
morp32cn <- read.csv(rule_source_path("morp2.csv"))
morp32 <- morp32en |>
  mutate(morp = paste0("M", substr(morp, 1, 6))) |>
  left_join(morp32cn, by = c("morp")) |>
  select(-mark, -behaviour) |>
  mutate(
    des = gsub("SPECIFIC SITES", "1", toupper(o3site)),
    morp2 = as.integer(substr(morp, 2, 5))
  ) |>
  left_join(allowed, by = c("morp2" = "morp")) |>
  rename(allowed = topo) |>
  left_join(nallowed, by = c("morp2" = "morp")) |>
  rename(nallowed = topo) |>
  select(morp, morp_desen, morp_descn, o3site, allowed, nallowed, comments)

dict_morp <- morp32

rm(allowed, nallowed, morp32en, morp32cn)

rex <- strsplit(morp32$morp_descn, split = "；")
names(rex) <- morp32$morp
