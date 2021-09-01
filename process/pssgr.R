library(readxl)
library(dplyr)
#% of type of user and price by route & direction at working day ----
TipoUsu <- read_xlsx("data/Anexo 6.1 - BD Subidas de Pasajeros Gran Valparaíso - Laboral VF.xlsx",
                     sheet = "Tipo de Pasajero", 
                     range = "B3:C10") %>%
  rename(CodPasaj = `Código Típo de Pasajero`, 
         Pasajero = `Tipo de Pasajero`)

BD2016 <- read_xlsx("data/Anexo 6.1 - BD Subidas de Pasajeros Gran Valparaíso - Laboral VF.xlsx",
                    sheet = "Base Datos",
                    skip = 3) %>% 
  select(c(1:22)) %>% 
  filter(row_number() <= 295873) %>%
  group_by(Servicio, 
           UN, 
           Sentido, 
           `Hora Subida`, 
           `Tarifa Cancelada`, 
           Pasajero) %>%
  summarise(Demanda = sum(`Factor Expansión`)) %>%
  rename(CodPasaj = Pasajero) %>%
  ungroup() %>%
  left_join(TipoUsu)

 