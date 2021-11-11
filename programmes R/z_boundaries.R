library(sf)
library(tidyverse)
# frontières les plus proches



requete_limites <- getbb("Andorre",featuretype = "country") %>%
  opq() %>%
  add_osm_feature(key="boundary")

limites <- osmdata_sf(requete_limites)$osm_multipolygons %>% st_as_sf(crs=4326) %>% st_transform(2154)
ggplot(data=limites)+
  geom_sf()

regions <- 
  
  mers <- 
  
  pays <- 