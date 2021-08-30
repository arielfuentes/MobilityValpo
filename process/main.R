#libraries ----
library(rmarkdown)
library(dplyr)
library(purrr)
library(sf)
library(tidyr)
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
#network data ----
source("process/inf5_wrg.R", encoding = "utf-8")
inf5_fl <- list.files("data/estraus/", full.names = T)
nt_dt <- lapply(inf5_fl, function(x) inf5(x)) %>%
  bind_rows() %>%
  mutate(per = case_when(fl %in% c("informe5_bus_valam_1.lpa", 
                                   "informe5_busmer_valam_1.lpa",
                                   "informe5_mer_valam_1.lpa") ~ "am1",
                         fl %in% c("informe5_bus_valam_2.lpa", 
                                   "informe5_busmer_valam_2.lpa",
                                   "informe5_mer_valam_2.lpa") ~ "am2",
                         fl %in% c("informe5_bus_valfp_1.lpa", 
                                   "informe5_busmer_valfp_1.lpa",
                                   "informe5_mer_valfp_1.lpa") ~ "fp",
                         fl %in% c("informe5_bus_valpt_1.lpa", 
                                   "informe5_busmer_valpt_1.lpa",
                                   "informe5_mer_valpt_1.lpa") ~ "pt1",
                         T ~ "otro")) %>%
  separate(idx_rt, c("idx", "rt"), sep = " ")
##adding fleet data ----
source("process/flt.R", encoding = "utf-8") 

lines_dt <- nt_dt %>%
  group_by(idx, per) %>%
  summarise(`DURACION RECORRIDO` = sum(`DURACION RECORRIDO`),
            `DISTANCIA RECORRIDO` = sum(`DISTANCIA RECORRIDO`),
            `PASAJEROS TRANSPORTADOS` = sum(`PASAJEROS TRANSPORTADOS`),
            `PAX * KM` = mean(`PAX * KM`),
            intervalo = mean(intervalo)) %>%
  left_join(fleet)
rm(fleet, inf5, inf5_fl)

#adding general parameters ----
source("process/Gl_param.R", encoding = "utf-8")
#calling process ----
render(input = "report/reportProcess.Rmd", 
       output_file = "Reporte Demanda", 
       output_dir = "report",
       encoding = "utf8")
