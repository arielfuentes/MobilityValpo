source("process/newtimes.R", encoding = "utf-8")
inf5_fl <- list.files("data/estraus/", full.names = T)
data_inf <- lapply(inf5_fl, function(x) extract_times(x)) %>%
  bind_rows() %>%
  mutate(per = case_when(fl %in% c("informe5_bus_valam_1.lpa", 
                                   "informe5_busmer_valam_1.lpa",
                                   "informe5_mer_valam_1.lpa") ~ "am1",
                         fl %in% c("informe5_bus_valam_2.lpa", 
                                   "informe5_busmer_valam_2.lpa",
                                   "informe5_mer_valam_2.lpa") ~ "am2",
                         fl %in% c("informe5_bus_valfp_1.lpa", 
                                   "informe5_busmer_valfp_1.lpa",
                                   "informe5_mer_valfp_1.lpa") ~ "fp",
                         fl %in% c("informe5_bus_valpt_1.lpa", 
                                   "informe5_busmer_valpt_1.lpa",
                                   "informe5_mer_valpt_1.lpa") ~ "pt1",
                         T ~ "otro"))

filter(data_inf, idx_rt %in% 
         c("9011 901", "9012 901", "3091 309", "3092 309", "9021 902", "9022 902")) %>%
  readr::write_delim(file = "data/new_times.csv", delim = ";")
