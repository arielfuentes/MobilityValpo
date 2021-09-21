library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

vel <- read_excel("../data/Parámetros.xlsx", sheet = "Velocidad")
temp <- read_excel("../data/Parámetros.xlsx", sheet = "T°") %>%
  filter(T_class %in% c("TXE", "TNJ")) %>%
  mutate(Sector_class = paste(T_class, SECTOR, sep = "_"))
temp_base <- select(temp, Sector_class, T_class, Temp = `LINEA BASE (1980 - 2010)`) %>%
  mutate(tipo = "Linea Base")
temp_2050 <- select(temp, Sector_class, T_class, Temp = `ESCENARIO 2050`) %>%
  mutate(tipo = "2050")
temp <- bind_rows(temp_base, temp_2050)
rm(temp_base, temp_2050)
acopios <- read_excel("../data/Parámetros.xlsx", 
                      sheet = "Acopios y Camello", range = "B2:BCL17") %>%
  pivot_longer(c(2:1441), names_to = "minuto") %>%
  mutate(minuto = as.numeric(minuto)) %>%
  rename(posición = hora_2)
acopios_Cab <- filter(acopios, str_starts(posición, "Plaza") | 
                     str_starts(posición, "Curauma"))
acopios_Circ <- filter(acopios, str_starts(posición, "Buses"))
acopios_Dep <- filter(acopios, str_starts(posición, "DepósitoC"))
Camello <- read_excel("../data/Parámetros.xlsx", 
                      sheet = "Acopios y Camello", range = "B19:C31")

# ggplot(vel, aes(hora, Velocidad)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~SS + factor(Tipo_día,
#                           levels = c("Laboral", "Sábado", "Domingo")),
#              nrow = 4) +
#   scale_x_continuous(breaks = seq(5,24, by = 2))

