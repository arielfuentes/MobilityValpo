kmz2sf <- function(kmz_path, path, exdir){
  library(fs)
  library(purrr)
  library(dplyr)
  library(sf)
  #unzip
  target <- paste0(path, ".temp.kml.zip")
  file_copy(kmz_path, target, overwrite = T)
  unzip(target, exdir = exdir)
  #read kml ----
  ##kml layers ----
  kml_dir <- paste0(path, "doc.kml")
  kml_layers <- st_layers(kml_dir)
  kml_fl <- kml_layers$name %>%
    map(function(x) read_sf(kml_dir, layer = x, quiet = T)) %>%
    bind_rows()
  return(kml_fl)
}

# kmz2sf(kmz_path = "data/trazado+concurso+valparaíso.kmz", 
#        path = "data/", 
#        exdir = paste0(getwd(), "/data"))  

