## code to prepare `create_logo` dataset goes here
library(hexSticker)
library(magick)
library(ggplot2)
library(dplyr)
library(tidyr)
# Convert the bitmap to a 'magick-image' object
pcs <- image_read("~/website/db/images/icon/dot.svg")
image <- image_fill(pcs,"none")
raster <- as.raster(image)

set.seed(123)
matrix_data <- matrix(sample(0:1, 100, replace = TRUE), ncol = 10)
set.seed(100)
dates <- seq.Date(as.Date("1945-06-07"),as.Date("1980-08-10"),
         length.out = 1000)
dates2 <- seq.Date(as.Date("2024-01-01"),as.Date("2024-12-31"),
                   length.out = 1000)
df <- data.frame(
  id = paste0("2024",sprintf("%03d",1:15)),
  birth =sample(dates, 15, replace = TRUE),
  inci = sample(dates2, 15, replace = TRUE),
  sex = sample(c(1,2), 15, replace = TRUE),
  topo = sample(gen_topo(0:809),15, replace = TRUE),
  morp = sample(gen_morp(0:9993),15, replace = TRUE),
  beha = rep(3, 15),
  grad = sample(c(1:9), 15, replace = TRUE)
  )
df <- data.frame(lapply(df, as.character), stringsAsFactors = FALSE)

df <- df %>%
  mutate(y = row_number()) %>%
  pivot_longer(cols = -y, names_to = "x", values_to = "value") |> 
  mutate(x= factor(x, levels= c("id", "birth", "inci","sex",
                                "topo", "morp", "beha", "grad")))


pp <- ggplot(df, aes(x = factor(x), y = y)) +
  geom_tile(fill = "transparent", color = "#b4b4b4") +  
  geom_text(aes(label = value), color = "#12211b", size = 4) +  
  theme_void() +  
  coord_fixed(ratio = 0.3) +
  theme(legend.position = "none")#+
  #annotation_raster(raster = raster, 1, 2, 3, 5)

sticker(pp,
        s_x = 1, s_y = 0.8,
        s_width = 1.2, s_height = 1,
        package = "iarccheck",
        p_y = 1.4,
        p_size = 50,
        p_color = "#27514c",
        p_family = "sans",
        p_fontface = "bold",
        h_size = 0.3,
        h_color = "#0f1e1d",
        h_fill = "#c1d1cf",
        filename = "logo.png",
        asp = 1,
        dpi = 600)

use_logo("logo.png")
