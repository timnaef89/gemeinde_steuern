
# get nachbarn (nur nachbarn, ohne gemeinde selbst für weitere stories)

library(tidyverse)
library(sf)


sf_gemeinden <- read_sf("swissboundaries3d_2023-01_2056_5728/g1g23.shp") %>% 
  select(GMDNAME, GMDNR, geometry) %>% 
  rename(NAME = GMDNAME,
         BFS_NUMMER = GMDNR)


i <- 1


sf_gemeinden$nachbarn_tmp <- sf_gemeinden %>% 
  st_intersects() 
#%>% 
  # sapply(function(x){
  # 
  #     x <- na.omit(ifelse(x == i, NA, x))
  #   
  #     i <- i + 1
  #   
  #   paste0(sf_gemeinden$NAME[x], collapse = ", ")
  # })

sf_gemeinden$nachbarn <- ""


for (i in 1:nrow(sf_gemeinden)) {

  nachbarn_int <- unlist(sf_gemeinden$nachbarn_tmp[i])
  
  x <- na.omit(ifelse(nachbarn_int == i, NA, nachbarn_int))
  
  
  
  
  sf_gemeinden$nachbarn[i] <- list(sf_gemeinden$BFS_NUMMER[x])

}

sf_gemeinden$nachbarn_name <- ""


for (i in 1:nrow(sf_gemeinden)) {
  
  nachbarn_int <- unlist(sf_gemeinden$nachbarn_tmp[i])
  
  x <- na.omit(ifelse(nachbarn_int == i, NA, nachbarn_int))
  
  
  
  
  sf_gemeinden$nachbarn_name[i] <- list(sf_gemeinden$NAME[x])
  
}


sf_gemeinden <- sf_gemeinden %>% 
  mutate(anzahl_nachbarn = str_count(nachbarn, ",")+1)

library(sf)

bfs_gemeinden_inklusive_nachbarn <- sf_gemeinden %>% 
  select(BFS_NUMMER, NAME, nachbarn_name, nachbarn, anzahl_nachbarn) 
# einkommentieren, wenn ohne geometry
# %>% 
#   st_drop_geometry()

write_rds(bfs_gemeinden_inklusive_nachbarn, "bfs_gemeinden_inklusive_nachbarn.rds")


################################################################################

# nachbarn mit gemeinde selbst --> für steuerstory



sf_gemeinden <- read_sf("swissboundaries3d_2023-01_2056_5728/g1g23.shp") %>% 
  select(GMDNAME, GMDNR, geometry) %>% 
  rename(NAME = GMDNAME,
         BFS_NUMMER = GMDNR)


i <- 1


sf_gemeinden$nachbarn_tmp <- sf_gemeinden %>% 
  st_intersects() 
#%>% 
# sapply(function(x){
# 
#     x <- na.omit(ifelse(x == i, NA, x))
#   
#     i <- i + 1
#   
#   paste0(sf_gemeinden$NAME[x], collapse = ", ")
# })

sf_gemeinden$nachbarn <- ""


for (i in 1:nrow(sf_gemeinden)) {
  
  nachbarn_int <- unlist(sf_gemeinden$nachbarn_tmp[i])
  
  x <- nachbarn_int
  
  
  
  
  sf_gemeinden$nachbarn[i] <- list(sf_gemeinden$BFS_NUMMER[x])
  
}

#sf_gemeinden$nachbarn_name <- ""


# for (i in 1:nrow(sf_gemeinden)) {
#   
#   nachbarn_int <- unlist(sf_gemeinden$nachbarn_tmp[i])
#   
#   x <- nachbarn_int
#   
#   
#   
#   
#   sf_gemeinden$nachbarn_name[i] <- list(sf_gemeinden$NAME[x])
#   
# }


sf_gemeinden <- sf_gemeinden %>% 
  mutate(anzahl_nachbarn = str_count(nachbarn, ","))

library(sf)

# bfs_gemeinden_inklusive_nachbarn <- sf_gemeinden %>% 
#   select(BFS_NUMMER, NAME, nachbarn_name, nachbarn, anzahl_nachbarn) 


sf_gemeinden <- sf_gemeinden %>% 
  st_drop_geometry() %>% 
  select(BFS_NUMMER, nachbarn, anzahl_nachbarn)

write_rds(sf_gemeinden, "nachbarn.RDS")


write_csv(sf_gemeinden, "nachbarsgemeinden.csv")

################################################################################

# variable mit nur nachbarsnamen


nachbarsgemeinden <- read_sf("swissboundaries3d_2023-01_2056_5728/g1g23.shp") %>% 
  select(GMDNAME, GMDNR, geometry) %>% 
  rename(NAME = GMDNAME,
         BFS_NUMMER = GMDNR)


nachbarsgemeinden$nachbarn_tmp <- sf_gemeinden %>% 
  st_intersects() 

i <- 1

nachbarsgemeinden$nachbarn_name <- ""



for (i in 1:nrow(nachbarsgemeinden)) {
  
  nachbarn_int <- unlist(nachbarsgemeinden$nachbarn_tmp[i])
  
  x <- na.omit(ifelse(nachbarn_int == i, NA, nachbarn_int))
  
  
  
  
  nachbarsgemeinden$nachbarn_name[i] <- list(nachbarsgemeinden$NAME[x])
  
}





nachbarsgemeinden$nachbarn_tmp <- nachbarsgemeinden %>% 
  st_intersects() %>%
  sapply(function(x){
    
    x <- na.omit(ifelse(x == i, NA, x))
    
    i <- i + 1
    
    paste0(nachbarsgemeinden$NAME[x], collapse = ", ")
  })

