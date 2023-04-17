
# get nachbarn

library(tidyverse)
library(sf)


sf_gemeinden <- read_sf("swissboundaries3d_2023-01_2056_5728/swissBOUNDARIES3D_1_4_TLM_HOHEITSGEBIET.shp")
i <- 1
sf_gemeinden$nachbarn <- sf_gemeinden %>% 
  st_intersects() %>% sapply(function(x){
    x <- na.omit(ifelse(x == i, NA, x))
    i <- i + 1
    paste0(sf_gemeinden$NAME[x], collapse = ", ")
  })
