rf_new_pred %>%
  group_by(per) %>%
  summarise(Demanda = sum(.pred))

rf_new_pred %>%
  group_by(per, UN) %>%
  summarise(Demanda = sum(.pred)) %>%
  ungroup() %>%
  group_by(per) %>%
  summarise(UN,
            `% Dem` = Demanda/sum(Demanda)*100,
            Demanda)

rf_new_pred %>%
  group_by(per, UN, Servicio) %>%
  summarise(Demanda = sum(.pred))

rf_new_pred %>%
  group_by(per, UN, Servicio, Pasajero) %>%
  summarise(Demanda = sum(.pred))

rf_new_pred %>%
  group_by(per, UN, Servicio, Pasajero, Tarifa) %>%
  summarise(Demanda = sum(.pred)) #%>%
  # filter(UN == 10)

rf_new_pred %>%
  filter(UN == "10" & per == "am2") %>%
  summarise(sum(.pred)/0.0732)
  
rf_new_pred %>%
  # filter(per == "am2") %>%
  group_by(UN, per) %>%
  summarise(Demanda = sum(Demanda)) %>%
  group_by(per) %>%
  summarise(UN,
            Demanda,
            `%` = Demanda/sum(Demanda)*100) %>%
  readr::write_delim("report/repartición_UN.csv", ";")

left_join(rf_new_pred, )

rf_new_pred %>%
  # select(-Demanda) %>%
  readr::write_delim("report/linea_per.csv", ";")
