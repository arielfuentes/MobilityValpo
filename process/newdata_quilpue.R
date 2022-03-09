library(readr)
filter(trips, 
       Servicio %in% c("104", "115", "120", "121", "122", "123", "125")) %>%
  mutate(TipoDia = factor(TipoDia, c("laboral", "sábado", "domingo"))) %>%
  group_by(TipoDia, Servicio, Sentido) %>%
  summarise(tviaje = mean(tviaje))

new_times <-  read_delim("data/tviaje_quilpue.csv", delim = ";") 
