library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
#% of type of user and price by route & direction at working day ----
TipoUsu <- read_xlsx("data/Anexo 6.1 - BD Subidas de Pasajeros Gran Valparaíso - Laboral VF.xlsx",
                     sheet = "Tipo de Pasajero", 
                     range = "B3:C10") %>%
  rename(CodPasaj = `Código Típo de Pasajero`, 
         Pasajero = `Tipo de Pasajero`)

# BD2016 <- read_xlsx("data/Anexo 6.1 - BD Subidas de Pasajeros Gran Valparaíso - Laboral VF.xlsx",
#                     sheet = "Base Datos",
#                     skip = 3,
                    # col_types = c(rep("guess", times = 3),
                    #               rep("text", times = 2),
                    #               rep("guess", times = 28)
                    # )
# ) %>% 
#   select(c(1:22)) %>% 
#   filter(row_number() <= 295873) %>%
#   mutate(Servicio = if_else(str_detect(Servicio, "-") == T,
#                             str_sub(Servicio, 1, nchar(Servicio) - 2), 
#                             Servicio)
#          ) %>%
#   group_by(Servicio, 
#            UN, 
#            Sentido, 
#            `Hora Subida`, 
#            `Tarifa Cancelada`, 
#            Pasajero) %>%
#   summarise(Demanda = sum(`Factor Expansión`)) %>%
#   rename(CodPasaj = Pasajero) %>%
#   ungroup() %>%
#   left_join(TipoUsu) %>%
#   mutate(Tarifa = case_when(`Tarifa Cancelada` == 0 ~ "Cero", 
#                             `Tarifa Cancelada` > 0 & `Tarifa Cancelada` <= 160 ~ "Baja",
#                             `Tarifa Cancelada` > 160 &  `Tarifa Cancelada` <= 300 ~ "Local",
#                             `Tarifa Cancelada` > 300 &  `Tarifa Cancelada` < 450 ~ "Media",
#                             `Tarifa Cancelada` >= 450 ~ "Alta",
#                             T ~ "Otro"),
#          Per = case_when(`Hora Subida` == 6 ~ "am1",
#                          `Hora Subida` == 7 ~ "am2",
#                          `Hora Subida` == 11 ~ "fp",
#                          `Hora Subida` == 18 ~ "pt1",
#                          T ~ "Otro")
#          ) %>%
#   select(-c("Hora Subida", "CodPasaj"))

BD2016 <- read_xlsx("data/Anexo 6.1 - BD Subidas de Pasajeros Gran Valparaíso - Laboral VF.xlsx",
                    sheet = "Base Datos",
                    skip = 3,
                    col_types = c(rep("skip", times = 3),
                                  rep("text", times = 2),
                                  rep("skip", times = 9),
                                  "numeric", 
                                  rep("skip", times = 3),
                                  rep("numeric", times = 3),
                                  rep("skip", times = 12))
                    ) %>% 
  filter(row_number() <= 295873) %>%
  mutate(Servicio = if_else(str_detect(Servicio, "-") == T,
                            str_sub(Servicio, 1, nchar(Servicio) - 2), 
                            Servicio)
  ) %>%
  group_by(Servicio, 
           UN, 
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
         per = case_when(`Hora Subida` == 6 ~ "am1",
                         `Hora Subida` == 7 ~ "am2",
                         `Hora Subida` == 11 ~ "fp",
                         `Hora Subida` == 18 ~ "pt1",
                         T ~ "Otro")
  ) %>%
  filter(per != "Otro") %>%
  select(-c("Hora Subida", "CodPasaj")) %>%
  group_by(Servicio, 
           UN, 
           per, 
           Tarifa, 
           Pasajero) %>%
  summarise(Demanda = sum(Demanda)) %>%
  ungroup()

rm(TipoUsu) 

# sharing <- BD2016 %>%
#   filter(Per != "Otro") %>%
#   select(-c("Sentido", "Tarifa Cancelada")) %>%
#   group_by(Servicio, 
#            UN, 
#            Per) %>%
#   mutate(`% particip` = Demanda/sum(Demanda)) %>%
#   ungroup()

#new data construction ----
serv <- pull(distinct(BD2016, Servicio))[1:106]
per <- pull(distinct(filter(BD2016, per != "Otro"), per))
Pas <- pull(distinct(BD2016, Pasajero))
Tarifa <- pull(distinct(BD2016, Tarifa))


sharing <- tibble(Tar = rep(Tarifa, each = 4),
                  per = rep(per, times = 5)) %>%
  mutate(Tar_per = paste(Tar, per, sep = "_"))


sharing <- tibble(Pas = rep(Pas, each = 20), 
               Tar_per = rep(sharing$Tar_per, times = 7)) %>%
  mutate(Pas_Tar_per = paste(Pas, Tar_per, sep = "_"))

sharing <- tibble(Servicio = rep(serv, each = 140), 
       Pas_Tar_per = rep(sharing$Pas_Tar_per, times = 106)) %>%
  separate(Pas_Tar_per, c("Pasajero", "Tarifa", "per"), sep = "_")

sharing <- left_join(sharing, BD2016) %>%
  select(-UN) %>%
  mutate(Demanda = if_else(is.na(Demanda), 0, Demanda)) %>%
  arrange(Servicio, per, Pasajero, Tarifa) %>%
  group_by(Servicio,
           per) %>%
  # mutate(`% particip` = Demanda/sum(Demanda)) %>%
  ungroup() %>%
  left_join(distinct(BD2016, Servicio, UN))

shar2 <- left_join(sharing, lines_dt) %>%
  na.omit() %>%
  select(-c("Pax Total", "Pax * Km", "Intervalo"))

new_data <- filter(shar2, Servicio %in% c("901", "902")) %>%
  mutate(Servicio = case_when(Servicio == "901" ~ "E01",
                              Servicio == "902" ~ "E02"),
         Demanda = 0,
         UN = "10",
         Duración = 6000,
         Frec = 0,
         Distancia = case_when(Servicio == "E01" ~ 46.79,
                               Servicio == "E02" ~ 48.05))
new_data_pred <- new_data %>%
  mutate(Frec = case_when(Servicio == "E01" & per == "am1" ~ 6,
                          Servicio == "E01" & per == "am2" ~ 12,
                          Servicio == "E01" & per == "fp" ~ 8,
                          Servicio == "E01" & per == "pt1" ~ 12,
                          Servicio == "E02" & per == "am1" ~ 2,
                          Servicio == "E02" & per == "am2" ~ 8,
                          Servicio == "E02" & per == "fp" ~ 8,
                          Servicio == "E02" & per == "pt1" ~ 11))
  