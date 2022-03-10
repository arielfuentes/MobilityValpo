library(readr)
tviaje_serday <- filter(trips, 
       Servicio %in% c("104", "115", "120", "121", "122", "123", "125")) %>%
  mutate(TipoDia = factor(TipoDia, c("laboral", "sábado", "domingo"))) %>%
  group_by(TipoDia, Servicio, Sentido) %>%
  summarise(tviaje = mean(tviaje)) %>%
  ungroup()

new_times <-  read_delim("data/tviaje_quilpue.csv", delim = ";") %>%
  rename(tviaje_new = tviaje)

delta_times <- left_join(tviaje_serday, new_times) %>%
  mutate(delta = tviaje_new/tviaje) %>%
  select(-c("tviaje", "tviaje_new"))

trips_new <- 
  trips %>%
  left_join(delta_times) %>%
  mutate(tviaje = if_else(!is.na(delta), tviaje*delta, tviaje)) %>%
  select(-delta)
