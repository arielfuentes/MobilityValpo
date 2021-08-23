#libraries ----
library(rmarkdown)
library(dplyr)
library(purrr)
library(sf)
source("process/kmz2sf.R", encoding = "utf-8")
#read kmz ----
elec_rt <- kmz2sf(kmz_path = "data/trazado+concurso+valparaíso.kmz",
                  path = "data/",
                  exdir = paste0(getwd(), "/data")) %>%
  select(-Description) %>%
  rename(Rutas = Name) %>%
  mutate(Rutas = c("E01I", "E01R", "E02NR", "E02I", "E02R"),
         Dist = units::set_units(st_length(.), "km"))
#elevation data ----
source("process/elev.R", encoding = "utf-8")
elec_rst <- elev_pts(elec_rt)
#read gpkg ----
rts_geo.h <- map(list.files("data/recorridos/", full.names = T), 
                 st_read) 
#calling process ----
render(input = "report/reportProcess.Rmd", 
       output_file = "Reporte Demanda", 
       output_dir = "report",
       encoding = "utf8")
