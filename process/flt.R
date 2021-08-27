library(readxl)
library(dplyr)
fleet_am1 <- readxl::read_excel("data/Flota.xlsm", sheet = "bus", range = "A2:B224") %>%
  mutate(per = "am1")
fleet_am2 <- readxl::read_excel("data/Flota.xlsm", sheet = "bus", range = "D2:E224") %>%
  mutate(per = "am2")
fleet_fp <- readxl::read_excel("data/Flota.xlsm", sheet = "bus", range = "G2:H224") %>%
  mutate(per = "fp")
fleet_pt1 <- readxl::read_excel("data/Flota.xlsm", sheet = "bus", range = "J2:K224") %>%
  mutate(per = "pt1")

fleet <- list(fleet_am1, fleet_am2, fleet_fp, fleet_pt1) %>%
  bind_rows()

rm(fleet_am1, fleet_am2, fleet_fp, fleet_pt1)