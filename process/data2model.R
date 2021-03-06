library(readxl)
library(dplyr)
library(tibble)
#new data construction ----
serv <- pull(distinct(DOI, Servicio))[1:106]
per <- pull(distinct(DOI, per))
Pas <- pull(distinct(DOI, Pasajero))
Tarifa <- pull(distinct(DOI, Tarifa))


sharing <- tibble(Tar = rep(Tarifa, each = 4),
                  per = rep(per, times = 5)) %>%
  mutate(Tar_per = paste(Tar, per, sep = "_"))


sharing <- tibble(Pas = rep(Pas, each = 20), 
                  Tar_per = rep(sharing$Tar_per, times = 7)) %>%
  mutate(Pas_Tar_per = paste(Pas, Tar_per, sep = "_"))

sharing <- tibble(Servicio = rep(serv, each = 140), 
                  Pas_Tar_per = rep(sharing$Pas_Tar_per, times = 106)) %>%
  separate(Pas_Tar_per, c("Pasajero", "Tarifa", "per"), sep = "_")

sharing <- left_join(sharing, DOI) %>%
  select(-UN) %>%
  mutate(Demanda = if_else(is.na(Demanda), 0, Demanda),
         Pasajero = if_else(Pasajero == "Niño Sin Uniforme",
                            "Escolar de Educación Básica",
                            Pasajero)
         ) %>%
  arrange(Servicio, per, Pasajero, Tarifa) %>%
  group_by(per,
           # UN,
           Servicio,
           Pasajero,
           Tarifa) %>%
  summarise(Demanda = sum(Demanda)) %>%
  ungroup() %>%
  left_join(distinct(DOI, Servicio, UN))

#final dataset ----
data_f <- left_join(sharing, lines_dt) %>%
  na.omit() %>%
  select(-c("Pax Total", "Pax * Km"))

#modify data for the scenario ----
# new_data <- filter(shar2, Servicio %in% c("901", "902")) %>%
#   mutate(Servicio = case_when(Servicio == "901" ~ "E01",
#                               Servicio == "902" ~ "1002"),
#          Demanda = 0,
#          UN = "10",
#          Duración = 6000,
#          Frec = 0,
         # Distancia = case_when(Servicio == "E01" ~ 46.79,
         #                       Servicio == "1002" ~ 48.05))
# new_data_pred <- new_data %>%
#   mutate(Frec = case_when(Servicio == "E01" & per == "am1" ~ 6,
#                           Servicio == "E01" & per == "am2" ~ 12,
#                           Servicio == "E01" & per == "fp" ~ 8,
#                           Servicio == "E01" & per == "pt1" ~ 12,
#                           Servicio == "1002" & per == "am1" ~ 2,
#                           Servicio == "1002" & per == "am2" ~ 8,
#                           Servicio == "1002" & per == "fp" ~ 8,
#                           Servicio == "1002" & per == "pt1" ~ 11))

new_data_pred <- data_f %>%
  mutate(Frec = case_when(Servicio == "1001" & per == "am1" ~ 6,
                          Servicio == "1001" & per == "am2" ~ 12,
                          Servicio == "1001" & per == "fp" ~ 8,
                          Servicio == "1001" & per == "pt1" ~ 12,
                          Servicio == "1002" & per == "am1" ~ 2,
                          Servicio == "1002" & per == "am2" ~ 8,
                          Servicio == "1002" & per == "fp" ~ 8,
                          Servicio == "1002" & per == "pt1" ~ 11,
                          T ~ Frec),
         Distancia = case_when(Servicio == "1001" ~ 46.79,
                               Servicio == "1002" ~ 48.05,
                               T ~ Distancia)
         )

new_times <- read_xlsx("../data/new_times.xlsx", sheet = "Hoja1") %>%
  separate(sersen, c("Servicio", "Sentido")) %>%
  group_by(Servicio, per) %>%
  summarise(Duración = sum(t)) %>%
  ungroup()

new_data_pred <- new_data_pred %>%
  mutate(Duración = case_when(Servicio == "1001" & per == "am1" ~ new_times$Duración[1],
                              Servicio == "1001" & per == "am2" ~ new_times$Duración[2],
                              Servicio == "1001" & per == "fp" ~ new_times$Duración[3],
                              Servicio == "1001" & per == "pt1" ~ new_times$Duración[4],
                              Servicio == "1002" & per == "am1" ~ new_times$Duración[5],
                              Servicio == "1002" & per == "am2" ~ new_times$Duración[6],
                              Servicio == "1002" & per == "fp" ~ new_times$Duración[7],
                              Servicio == "1002" & per == "pt1" ~ new_times$Duración[8],
                              T ~ Duración))

# new_data_pred <- new_data_pred %>%
#   mutate(Duración = case_when(Servicio == "1001" & per == "am1" ~ 82.9213508,
#                               Servicio == "1001" & per == "am2" ~ 100.142836,
#                               Servicio == "1001" & per == "fp" ~ 84.9513805,
#                               Servicio == "1001" & per == "pt1" ~ 92.0544982,
#                               Servicio == "1002" & per == "am1" ~ 84.9833801,
#                               Servicio == "1002" & per == "am2" ~ 99.9532478,
#                               Servicio == "1002" & per == "fp" ~ 87.3646872,
#                               Servicio == "1002" & per == "pt1" ~ 93.8419968,
#                               T ~ Duración))

rm(new_times, sharing, DOI, lines_dt)
