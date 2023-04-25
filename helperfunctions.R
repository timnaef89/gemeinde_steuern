
# get nachbarn

library(tidyverse)
library(sf)


sf_gemeinden <- read_sf("swissboundaries3d_2023-01_2056_5728/g1g23.shp") %>% 
  select(GMDNAME, GMDNR, geometry) %>% 
  rename(NAME = GMDNAME,
         BFS_NUMMER = GMDNR)


i <- 1


sf_gemeinden$nachbarn <- sf_gemeinden %>% 
  st_intersects() %>% 
  sapply(function(x){
  
     # x <- na.omit(ifelse(x == i, NA, x))
    
      i <- i + 1
    
    paste0(sf_gemeinden$NAME[x], collapse = ", ")
  })


for (i in 1:nrow(sf_gemeinden)) {

  sf_gemeinden$nachbarn[i] <- gsub(paste0(sf_gemeinden$NAME[i], ", "),"", sf_gemeinden$nachbarn[i])

}

sf_gemeinden <- sf_gemeinden %>% 
  mutate(anzahl_nachbarn = str_count(nachbarn, ",")+1)

library(sf)

sf_gemeinden <- sf_gemeinden %>% 
  st_drop_geometry() %>% 
  select(BFS_NUMMER, nachbarn, anzahl_nachbarn)


write_csv(sf_gemeinden, "nachbarsgemeinden.csv")
