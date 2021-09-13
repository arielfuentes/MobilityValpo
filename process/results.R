rf_new_pred %>%
  group_by(per) %>%
  summarise(Demanda = sum(.pred))

rf_new_pred %>%
  group_by(per, UN) %>%
  summarise(Demanda = sum(.pred)) %>%
  ungroup() %>%
  group_by(per) %>%
  summarise(`% Dem` = Demanda/sum(Demanda)*100,
            UN,
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
