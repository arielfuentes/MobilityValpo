inf5 <- function(lpa_fl){
  #libraries
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  #reading file ----
  lpa <- read_csv(lpa_fl, skip = 1)
  names(lpa) <- "col1"
  #filtering data & add route id ----
  lpa <- lpa %>%
    mutate(idx = if_else(str_detect(lpa$col1, "^a") == T, 1, 0))
  ##create groups ----
  for (i in 2:length(lpa$idx)){
    lpa$idx[i] <- lpa$idx[i] + lpa$idx[i-1]
  }
  ##split by groups ----
  lpa <- group_split(lpa, idx)
  #tbl w/id & intervals by min ----
  ##spliting data ----
  id_tbl <- lapply(lpa, function(x) filter(x, 
                                               str_starts(x$col1, 
                                                          c("^L"))) %>%
                     separate(col = col1, into = LETTERS[1:5], sep = ":") %>%
                     select(B, D) %>%
                     separate(B, c("w", "rt_id", "rt", "sen")) %>%
                     separate(D, c("w2", "num", "dec")) %>%
                     mutate(intervalo = as.numeric(paste0(num, ".", dec)),
                            idx_rt = paste(rt_id, rt)) %>%
                     select(idx_rt, rt_id, rt, sen, intervalo)) 
  ##right structure for interval data ----
  id_tbl2 <- lapply(id_tbl, function(x) mutate(bind_rows(x, x), 
                                              sen = c("Ida", "Regreso")
                                              )
                   ) %>%
    bind_rows() %>% 
    mutate(vrbl = "intervalo") %>% 
    select(valor = intervalo, idx_rt, vrbl, sen)
  
  #extract routes kpi
  params <- lapply(lpa, function(x) filter(x, str_starts(x$col1, c("^DU|^DISTANCIA R|^PAS|^PAX")
                                                         )
                                           )
                   )
  params <- lapply(1:length(params), function(x) select(params[[x]], col1))
  #change structure of id_tbl ----
  id_tbl <- id_tbl %>%
    bind_rows() %>% 
    mutate(id_tbl, vrbl = "intervalo") %>% 
    select(valor = intervalo, idx_rt, vrbl, sen)
  #main output ----
  ##extracting numbers ----
  ##creating tibble & assemble ----
  ##ordering data ----
  params_tbl <- lapply(1:length(params), 
                       function(x) tibble(valor = na.omit(as.numeric(strsplit(as.character(params[[x]]), " ")[[1]]
                                                                     )
                                                          ),
                                          idx_rt = rep(id_tbl[x, ]$idx_rt, 8),
                                          vrbl = rep(c("DURACION RECORRIDO",
                                                       "DISTANCIA RECORRIDO",
                                                       "PASAJEROS TRANSPORTADOS",               
                                                       "PAX * KM"), 
                                                     each = 2),
                                          sen = rep(c("Ida", "Regreso"), 4)
                                          )
                       ) %>%
    bind_rows() %>%
    bind_rows(id_tbl2) %>%
    arrange(idx_rt, sen) %>%
    mutate(fl = str_split(lpa_fl, "/")[[1]][3]) %>%
    pivot_wider(names_from = vrbl, values_from = valor) %>%
    filter(`DURACION RECORRIDO` != 0 &
             `DISTANCIA RECORRIDO` != 0 & 
             `PASAJEROS TRANSPORTADOS` != 0 & 
             `PAX * KM` != 0)
  #final table ----
  return(params_tbl)
}

# inf5("data/estraus/informe5_bus_valam_1.lpa")