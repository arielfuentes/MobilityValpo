library(readr)
library(dplyr)
library(stringr)
library(tidyr)
#read inf5 ----
valam_1 <- read_csv("data/estraus/informe5_bus_valam_1.lpa", skip = 1)
names(valam_1) <- "col1"
valam_1 <- valam_1 %>%
  mutate(idx = if_else(str_detect(valam_1$col1, "^a") == T, 1, 0)
         )
##create groups ----
for (i in 2:length(valam_1$idx)){
  valam_1$idx[i] <- valam_1$idx[i] + valam_1$idx[i-1]
}

##split by groups ----
valam_1 <- group_split(valam_1, idx)

desc <- lapply(valam_1, function(x) filter(x, 
                                   str_starts(x$col1, 
                                              c("^L"))) %>%
         separate(col = col1, into = LETTERS[1:5], sep = ":") %>%
         select(B, D) %>%
         separate(B, c("w", "rt_id", "rt", "sen")) %>%
         separate(D, c("w2", "num", "dec")) %>%
         mutate(intervalo = as.numeric(paste0(num, ".", dec)),
                idx_rt = paste(rt_id, rt)) %>%
         select(idx_rt, rt_id, rt, sen, intervalo))

filter(valam_1[[1]], 
       str_starts(valam_1[[1]]$col1, 
                  c("^DU|^DISTANCIA R|^PAS|^NU|^PAX"))) %>%
  separate(col1, c("name", "I"), sep = ":") %>%
  separate(I, c("A", "B", "C", "D", "E"))
filter(valam_1[[1]], str_starts(valam_1[[1]]$col1, "NO") == T)
filter(valam_1[[1]], str_starts(valam_1[[1]]$col1, "TI") == T)
filter(valam_1[[1]], str_starts(valam_1[[1]]$col1, "DISTANCIA :") == T)
filter(valam_1[[1]], str_starts(valam_1[[1]]$col1, "FL") == T)
filter(valam_1[[1]], str_starts(valam_1[[1]]$col1, "TASA USO  :") == T)
filter(valam_1[[1]], str_starts(valam_1[[1]]$col1, "S") == T)
filter(valam_1[[1]], str_starts(valam_1[[1]]$col1, "B") == T)

filter(valam_1[[1]], str_starts(valam_1[[1]]$col1, c("NO", "TI", "FL", "TASA USO  :", "S", "B")) == T)
filter(valam_1[[1]], str_starts(valam_1[[1]]$col1, c("^NO|^TI|^DISTANCIA :|^FL|^TASA USO  :|^S|^B")) == T)

x <- filter(valam_1[[1]], 
       str_starts(valam_1[[1]]$col1, 
                  c("^L"))) %>%
  separate(col = col1, into = LETTERS[1:5], sep = ":") %>%
  select(B, D) %>%
  separate(B, c("w", "rt_id", "rt", "sen")) %>%
  separate(D, c("w2", "num", "dec")) %>%
  mutate(intervalo = as.numeric(paste0(num, ".", dec)),
         idx_rt = paste(rt_id, rt)) %>%
  select(idx_rt, rt_id, rt, sen, intervalo)
# stringr::str_remove(x, 
#                     c("LINEA IDA| LINEA REGRESO | INTERVALO | TARIFA"))  %>%
#   as_tibble() %>%
#   tidyr::separate(c("LINEA", "MODO", "CAPACIDAD", "TARIFA"), sep = ":")



# data.frame(x = c(NA, "a.b", "a.d", "b.c")) %>% separate(x, c("A", "B"))
