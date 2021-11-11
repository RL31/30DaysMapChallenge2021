library(osmdata)
library(tidyverse)
library(sf)


requete_man_made <- getbb("France",featuretype = "country") %>%
  opq(timeout = 1000000) %>%
  add_osm_feature(key="man_made",value="water_tower")
requete_building <- getbb("France") %>%
  opq(timeout = 100000) %>%
  add_osm_feature(key="building",value="water_tower")

chateaux_deau1 <- osmdata_sf(requete_man_made) 
chateaux_deau2 <- osmdata_sf(requete_building) 

chateaux_deau <- chateaux_deau1$osm_points %>% 
  bind_rows(chateaux_deau1$osm_polygons %>% st_centroid()) %>% 
  bind_rows(chateaux_deau2$osm_points %>% st_centroid()) %>% 
bind_rows(chateaux_deau2$osm_polygons %>% st_centroid())


ggplot()+
  geom_sf(data=chateaux_deau2)+
  theme_void()
