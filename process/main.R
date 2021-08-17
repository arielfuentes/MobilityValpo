#libraries ----
library(rmarkdown)
library(dplyr)
source("process/kmz2sf.R", encoding = "utf-8")
#read kmz ----
elec_rt <- kmz2sf(kmz_path = "data/trazado+concurso+valparaíso.kmz",
                  path = "data/",
                  exdir = paste0(getwd(), "/data")) %>%
  select(-Description) %>%
  rename(Rutas = Name) %>%
  mutate(Rutas = c("E01I", "E01R", "E02NR", "E02I", "E02R"),
         Dist = units::set_units(st_length(.), "km"))
#calling process ----
render(input = "report/reportProcess.Rmd", 
       output_file = "Reporte Demanda", 
       output_dir = "report",
       encoding = "utf8")
