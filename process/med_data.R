library(DBI)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
# connect_to_access_dbi <- function(db_file_path)  {
#   require(DBI)
#   # make sure that the file exists before attempting to connect
#   if (!file.exists(db_file_path)) {
#     stop("DB file does not exist at ", db_file_path)
#   }
#   # Assemble connection strings
#   dbq_string <- paste0("DBQ=", db_file_path)
#   driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
#   db_connect_string <- paste0(driver_string, dbq_string)
#   
#   myconn <- dbConnect(odbc::odbc(),
#                       .connection_string = db_connect_string,
#                       encoding = "latin1")
#   return(myconn)
# }
# 
# con <- connect_to_access_dbi("data/Anexo 6.5 - BD Buses Gran Valparaiso VF.accdb")
con <- 
  dbConnect(odbc::odbc(),
            .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=data/Anexo 6.5 - BD Buses Gran Valparaiso VF.accdb",
            encoding = "latin1")
sql <- "SELECT TipoServicio.TipoServicio, 
Subida_Pasajeros.FechaID AS Fecha,
Dia.TipoDia, 
Hour(Salida_Buses.HoraSalida) AS HoraInicio,
Subida_Pasajeros.ServicioID AS Servicio, 
sentido.[ID Sentido] AS Sentido, 
Subida_Pasajeros.CorrelSalidaBusID,
Subida_Pasajeros.FechaID &' '& Salida_Buses.HoraSalida AS HoraSalida,
Subida_Pasajeros.FechaID &' '& Salida_Buses.HoraLlegada AS HoraLlegada,
Tipo_Pasajero.DescripcionTipoPas AS TipoPax,
Subida_Pasajeros.TarifaPagada AS Tarifa,       
SUM([Frecuencia]/[Muestra]) AS Demanda
FROM Dia INNER JOIN 
  (((Salida_Buses INNER JOIN 
    ((((Subida_Pasajeros 
    INNER JOIN 
        Factores_Expansion ON 
        (Subida_Pasajeros.PatenteID = Factores_Expansion.PatenteID) AND 
        (Subida_Pasajeros.ServicioID = Factores_Expansion.ServicioID) AND 
        (Subida_Pasajeros.FechaID = Factores_Expansion.FechaID) AND 
        (Subida_Pasajeros.SentidoID = Factores_Expansion.SentidoID) AND 
        (Subida_Pasajeros.CorrelSalidaBusID = Factores_Expansion.CorrelSalidaBusID)) INNER JOIN 
          TipoServicio ON 
          Subida_Pasajeros.ServicioID = TipoServicio.ServicioID)
          INNER JOIN Tipo_Pasajero ON 
          Subida_Pasajeros.TipoPasajero = Tipo_Pasajero.TipoPasajeroID)
          INNER JOIN sentido ON
          Subida_Pasajeros.SentidoID = sentido.identificador) ON
          (Salida_Buses.CorrelSalidaBusID = Subida_Pasajeros.CorrelSalidaBusID) AND 
          (Salida_Buses.SentidoID = Subida_Pasajeros.SentidoID) AND 
          (Salida_Buses.FechaID = Subida_Pasajeros.FechaID) AND 
          (Salida_Buses.ServicioID = Subida_Pasajeros.ServicioID) AND 
          (Salida_Buses.PatenteID = Subida_Pasajeros.PatenteID) AND 
          (Salida_Buses.ServicioID = TipoServicio.ServicioID)))) ON 
          Dia.DiaID = Salida_Buses.Dia
GROUP BY TipoServicio.TipoServicio, 
Dia.TipoDia, 
Subida_Pasajeros.FechaID,
Hour(Salida_Buses.HoraSalida),
Subida_Pasajeros.ServicioID, 
sentido.[ID Sentido], 
Subida_Pasajeros.CorrelSalidaBusID,
Subida_Pasajeros.FechaID &' '& Salida_Buses.HoraSalida,
Subida_Pasajeros.FechaID &' '& Salida_Buses.HoraLlegada,
Tipo_Pasajero.DescripcionTipoPas,
Subida_Pasajeros.TarifaPagada;"

trips <- dbGetQuery(conn = con,
                    statement = sql) %>%
  as_tibble()
dbDisconnect(con)
trips <- filter(trips, TipoPax != "Movilidad Reducida" & TipoDia != "lunes"
                & str_detect(Servicio, "-") == F
                ) %>%
  mutate(HoraSalida = as.POSIXct(HoraSalida, 
                                 format = "%d-%m-%Y %H:%M:%S", 
                                 tz = "UTC"),
         HoraLlegada = as.POSIXct(HoraLlegada, 
                                  format = "%d-%m-%Y %H:%M:%S", 
                                  tz = "UTC"),
         tviaje = as.numeric(as.duration(interval(HoraSalida, HoraLlegada)),
                             "minutes"),
         Recaudación = Demanda*Tarifa,
         TipoPax = if_else(TipoPax == "Niño", "Escolar Básica", TipoPax),
         CatUsu = if_else(TipoPax == "Adulto", "Adulto", "Otro"),
         TipoRec = factor(case_when(Servicio %in% c("104", "104-1", "115", "122", "123" ) ~ 2,
                             Servicio %in% c("120", "121", "121-1", "125" ) ~ 1,
                             T ~ 0))
) %>%
  group_by(TipoServicio, TipoDia, Servicio, Sentido, Fecha, HoraInicio, TipoRec,
           # CorrelSalidaBusID, 
           TipoPax, Tarifa) %>%
  summarise(tviaje = mean(tviaje), Demanda = sum(Demanda), Recaudación = sum(Recaudación)) %>%
  ungroup()

trips %>%
  filter(TipoServicio == "Urbano" & 
           # TipoDia != "lunes" &
           TipoPax == "Adulto"
           ) %>%
  ggplot(aes(y = Demanda, x = factor(HoraInicio))) +
  geom_boxplot() +
  facet_grid(rows = vars(TipoRec), cols = vars(TipoDia))

trips %>%
  filter(TipoServicio == "Urbano" & 
           TipoDia != "lunes" &
           TipoPax != "Adulto"
  ) %>%
  ggplot(aes(y = Demanda, x = factor(HoraInicio))) +
  geom_boxplot() +
  facet_grid(rows = vars(TipoRec), cols = vars(TipoDia))

trips %>%
  filter(TipoServicio == "Urbano" & 
           TipoDia != "lunes" &
           TipoPax == "Adulto"
  ) %>%
  ggplot(aes(y = Demanda, x = factor(HoraInicio))) +
  geom_violin() +
  facet_grid(rows = vars(TipoRec), cols = vars(TipoDia))

trips %>%
  filter(TipoServicio == "Urbano" & 
           TipoDia != "lunes" &
           TipoPax != "Adulto"
  ) %>%
  ggplot(aes(y = Demanda, x = factor(HoraInicio))) +
  geom_violin() +
  facet_grid(rows = vars(TipoRec), cols = vars(TipoDia))

trips %>%
  filter(TipoServicio == "Urbano" & 
           TipoDia != "lunes" &
           TipoPax != "Adulto"
  ) %>%
  ggplot(aes(x = Demanda, fill = factor(HoraInicio))) +
  geom_density(alpha = .5) +
  facet_wrap(~ TipoPax)

trips %>%
  filter(TipoServicio == "Urbano" & TipoDia != "lunes") %>%
  ggplot(aes(y = Demanda, x = log1p(tviaje), color = TipoPax)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, color = "black") +
  facet_grid(rows = vars(TipoRec), cols = vars(TipoDia))
