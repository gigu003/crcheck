## code to prepare `cases` dataset goes here
library(dplyr)
age <- c(0:110)
sex <- ValueRange$iarc$sex
topo <- ValueRange$iarc$topo
morp <- ValueRange$iarc$morp
beha <- ValueRange$iarc$beha[1:4]
grad <- ValueRange$iarc$grad
basi <- ValueRange$iarc$basi

# 随机生成样本（抽样而不生成完整的组合表）
set.seed(20) # 设置随机种子以便复现

# part 1 age_topo_morp 
age_topo_morp <- expand.grid(age = age, topo = topo, morp = morp)
s_size <- nrow(age_topo_morp)
t_size <- t_size+s_size
age_topo_morp <- age_topo_morp |>
  mutate(registr = sprintf("%010d", row_number() + t_size))


# part 2 topo_morp
topo_morp <- expand.grid(topo = topo, morp = morp)
s_size <- nrow(topo_morp)
topo_morp <- topo_morp |>
  mutate(registr = sprintf("%010d", row_number() + t_size))
t_size <- t_size+s_size


# part 3 sex_topo
sex_topo <- expand.grid(sex = sex, topo = topo)
s_size <- nrow(sex_topo)
sex_topo <- sex_topo |>
  mutate(registr = sprintf("%010d", row_number() + t_size))
t_size <- t_size+s_size

# part 4 sex_morp
sex_morp <- expand.grid(sex = sex, morp = morp)
s_size <- nrow(sex_morp)
sex_morp <- sex_morp |>
  mutate(registr = sprintf("%010d", row_number() + t_size))
t_size <- t_size+s_size

# part 5 morp_grad
morp_grad <- expand.grid(morp = morp, beha = beha, grad = grad)
s_size <- nrow(morp_grad)
morp_grad <- morp_grad |>
  mutate(registr = sprintf("%010d", row_number() + t_size))
t_size <- t_size+s_size

# part 6 morp_beha
morp_beha <- expand.grid(morp = morp, beha = beha)
s_size <- nrow(morp_beha)
morp_beha <- morp_beha |>
  mutate(registr = sprintf("%010d", row_number() + t_size))
t_size <- t_size+s_size

# part 7 basi_morp
basi_morp <- expand.grid(basi = basi, morp = morp)
s_size <- nrow(basi_morp)
basi_morp <- basi_morp |>
  mutate(registr = sprintf("%010d", row_number() + t_size))
t_size <- t_size+s_size

# part 8 beha_topo
beha_topo <- expand.grid(topo = topo, morp = morp, beha = beha)
s_size <- nrow(beha_topo)
beha_topo <- beha_topo |>
  mutate(registr = sprintf("%010d", row_number()+t_size))
t_size <- t_size+s_size


saveRDS(age_topo_morp, "age_topo_morp.rds")
saveRDS(basi_morp, "basi_morp.rds")
saveRDS(beha_topo, "beha_topo.rds")
saveRDS(morp_beha, "morp_beha.rds")
saveRDS(morp_grad, "morp_grad.rds")
saveRDS(sex_morp, "sex_morp.rds")
saveRDS(sex_topo, "sex_topo.rds")
saveRDS(topo_morp, "topo_morp.rds")


