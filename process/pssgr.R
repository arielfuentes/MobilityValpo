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
                    skip = 3,
                    col_types = c(rep("guess", times = 3),
                                  rep("text", times = 2), 
                                  rep("guess", times = 28)
                    )
) %>% 
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
  left_join(TipoUsu) %>%
  mutate(Tarifa = case_when(`Tarifa Cancelada` == 0 ~ "Cero", 
                            `Tarifa Cancelada` > 0 & `Tarifa Cancelada` <= 160 ~ "Baja",
                            `Tarifa Cancelada` > 160 &  `Tarifa Cancelada` <= 300 ~ "Local",
                            `Tarifa Cancelada` > 300 &  `Tarifa Cancelada` < 450 ~ "Media",
                            `Tarifa Cancelada` >= 450 ~ "Alta",
                            T ~ "Otro"),
         Per = case_when(`Hora Subida` == 6 ~ "am1",
                         `Hora Subida` == 7 ~ "am2",
                         `Hora Subida` == 11 ~ "fp",
                         `Hora Subida` == 18 ~ "pt1",
                         T ~ "Otro")
  )

 