extract_times <- function(lpa_fl){
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
                     select(idx_rt, sen)) 
  
  #extract routes kpi
  params <- lapply(lpa, function(x) filter(x, 
                                           str_starts(x$col1, c("^NO|^TI"))))
  
  params <- lapply(1:length(params), function(x) select(params[[x]], col1))
  
  
  
  #main output ----
  
  
  data <- lapply(1:length(params), 
         function(x) bind_cols(params[[x]], 
                               tibble(idx_rt = rep(id_tbl[[x]]$idx_rt,
                                                   times = nrow(params[[x]])),
                                      sen = rep(id_tbl[[x]]$sen,
                                                times = nrow(params[[x]]))))) %>%
    bind_rows() %>%
    mutate(fl = str_split(lpa_fl, "/")[[1]][3])
  return(data)
}