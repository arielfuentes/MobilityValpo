library(readxl)
library(dplyr)
library(sf)
library(tmap)
#create network ----
##call network data points ---- 
red_vial <- read_excel("data/Red Valparaiso.xlsm", 
                       sheet = "nodos_geo", 
                       range = "B1:E5126") %>%
  filter(Tipo == "Red Vial")
red_merval <- read_excel("data/Red Valparaiso.xlsm", 
                         sheet = "nodos_geo", 
                         range = "J1:M59")
red <- bind_rows(red_vial, red_merval)
rm(red_vial, red_merval)

# red_rt <- red %>%
#   filter(Tipo == "Red Vial") %>%
#   mutate(id = c(rep(seq(1, 2372), each = 2), 2373)) %>%
#   st_as_sf(coords = c("X", "Y"), crs = 32719) %>%
#   group_by(id) %>%
#   summarise(ID = mean(`ID Final`)) %>%
#   st_cast("LINESTRING")
# red %>%  
#   rename(ID_i = `ID Final`, Xi = X, Yi = Y) %>%
#   mutate(ID_f = lead(ID_i), Xf = lead(Xi), Yf = lead(Yi))
## create points map ----
red_pt <- st_as_sf(red, coords = c("X", "Y"), crs = 32719)

tm_shape(red_pt) +
  tm_dots("Tipo")
