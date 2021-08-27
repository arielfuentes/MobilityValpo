library(readxl)

vel <- read_excel("data/Parámetros.xlsx", sheet = "Velocidad") 
temp <- read_excel("data/Parámetros.xlsx", sheet = "T°")
acopios <- read_excel("data/Parámetros.xlsx", 
                      sheet = "Acopios y Camello", range = "B2:BCL17")
Camello <- read_excel("data/Parámetros.xlsx", 
                      sheet = "Acopios y Camello", range = "B19:C31")
