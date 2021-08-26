#f(x) 2 get elevation data ----
##based on linestring ----
elev_pts <- function(ln_fl){
  #libraries
  library(elevatr)
  library(dplyr)
  library(sf)
  library(tidyr)
  #transform 2 points ----
  pt <- ln_fl %>%
    st_transform(32719) %>%
    st_cast("POINT") %>%
    select(-Dist) %>%
    group_by(Rutas) %>%
    mutate(Orden = row_number()) %>%
    ungroup()
  #get elevation pts ----
  elev_ptx <- get_elev_point(pt, src = "aws", z = 14) %>%
    mutate(x = st_coordinates(.)[, 1],
           y = st_coordinates(.)[, 2],
           distancia = sqrt((y-lag(y))^2+(x-lag(x))^2),
           distancia = replace_na(distancia, 0),
           `% elev` = (elevation - lag(elevation))/distancia*100,
           `% elev` = replace_na(`% elev`, 0))
  return(elev_ptx)
}

# elec_rst <- elev_pts(elec_rt)
# readr::write_delim("report/elev.csv", x = elec_rst, delim = ";")