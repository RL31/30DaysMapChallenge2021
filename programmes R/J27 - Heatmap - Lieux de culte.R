library(osmdata)
library(tidyverse)
library(sf)
library(btb)
library(viridis)



regions1 <- c("Occitanie",
              "Provence-Alpes-Côte d'Azur",
              "Nouvelle-Aquitaine",
              "Auvergne-Rhône-Alpes")
regions2 <- c("Bretagne",
              "Normandie",
              "Pays de la Loire",
              "Centre-Val-de-Loire")
regions3 <- c("Bourgogne-Franche-Comté",
              "Grand Est",
              "Ile-de-France",
              "Hauts-de-France",
              "Corse")


requete_worship <-  opq(bbox=c(-5.405273,41.079351,10.107422,51.481383),
                        # Belgique 1.922607,49.303636,6.712646,51.774638),
                        # #Suisse : 5.493164,45.759859,10.722656,47.761484),
                        timeout = 100000) %>%
  add_osm_feature(key="amenity",value="place_of_worship")

culte <- osmdata_sf(requete_worship) 

donnees <- culte$osm_points %>%
  st_transform(2154) %>% 
  st_centroid() %>% 
  mutate(x=st_coordinates(.)[,1],
         y=st_coordinates(.)[,2],
         protestant=case_when(denomination=="protestant" ~ 1,
                              TRUE ~ 0),
         lieu = 1) %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  select(lieu,protestant,x, y)



cultes_par_regions <- function(REGION){
  requete_worship <-  getbb(REGION) %>% 
    opq(timeout = 100000) %>%
    add_osm_feature(key="amenity",value="place_of_worship")
  
  culte <- osmdata_sf(requete_worship) 
  
  culte2 <- culte$osm_polygons %>%
    st_transform(2154) %>% 
    st_centroid() %>% 
    mutate(x=st_coordinates(.)[,1],
           y=st_coordinates(.)[,2],
           protestant=case_when(denomination=="protestant" ~ 1,
                                TRUE ~ 0),
           lieu = 1) %>% 
    st_drop_geometry() %>% 
    as.data.frame() %>% 
    select(lieu,protestant,x, y)
  
  return(culte2)
  
}

# regions <- c("Corse","Bretagne")
# 
# lieux_de_culte1 <- map_df(regions1, cultes_par_regions)
# lieux_de_culte2 <- map_df(regions2, cultes_par_regions)
# lieux_de_culte3 <- map_df(regions3, cultes_par_regions)
# lieux_BFC <- cultes_par_regions("Bourgogne-Franche-Comté")
# lieux_C <- cultes_par_regions("Corse")
# lieux_GE <- cultes_par_regions("Grand Est")
# lieux_HDF <- cultes_par_regions("Hauts-de-France")
# lieux_IDF <- cultes_par_regions("Ile-de-France")
# 
# lieux_de_culte <- lieux_de_culte1 %>% 
#   bind_rows(lieux_de_culte2) %>% 
#   bind_rows(lieux_GE) %>% 
#   bind_rows(lieux_HDF) %>% 
#   bind_rows(lieux_IDF) %>% 
#   bind_rows(lieux_BFC) %>% 
#   bind_rows(lieux_C) %>% 
#   distinct()
# 
# saveRDS(lieux_de_culte,"donnees/place_of_worship.RDS")


complement1 <- cultes_par_regions("Luxembourg")
complement2 <- cultes_par_regions("Royaume de Belgique")
complement3 <- cultes_par_regions("Baden-Wurtemberg")
complement4 <- cultes_par_regions("Rheinland-Pfalz")
complement5 <- cultes_par_regions("Saarland")
complement6 <- cultes_par_regions("Confédération suisse")
complement7 <- cultes_par_regions("Angleterre")
complement8 <- cultes_par_regions("Catalonia")
complement9 <- cultes_par_regions("Aragon")
complement10 <- cultes_par_regions("Navarra")
complement11 <- cultes_par_regions("Piemonte")
complement12 <- cultes_par_regions("Hessen")

lieux_de_culte <- readRDS("donnees/place_of_worship.RDS") %>% 
  bind_rows(complement1) %>% 
  bind_rows(complement2) %>% 
  bind_rows(complement3) %>% 
  bind_rows(complement4) %>% 
  bind_rows(complement5) %>% 
  bind_rows(complement6) %>% 
  bind_rows(complement7) %>% 
  bind_rows(complement8) %>% 
  bind_rows(complement9) %>% 
  bind_rows(complement10) %>% 
  bind_rows(complement11) %>% 
  distinct()
# st_as_sf(coords=c("x","y"),crs=2154)

saveRDS(lieux_de_culte,"donnees/place_of_worship.RDS")


regions <- st_read("donnees/REGION.shp") %>% 
  summarise()

lissage_FR <- kernelSmoothing(dfObservations = lieux_de_culte,# donnees,
                              sEPSG = 2154,
                              iCellSize = 2000, #2000
                              iBandwidth = 20000) %>% 
  mutate(taux=round(protestant/lieu*100,0),
         taux_tr=cut(taux,breaks =c(0,1,5,10,15,20,25,50,75,100),include.lowest = TRUE ) ) %>% 
  group_by(taux_tr) %>% 
  summarise()

europe <- st_read("donnees/CNTR_RG_10M_2020_3035.shp") %>%
  filter(CNTR_ID %in% c("FR","UK","BE","LU","NL","ES","AD","IT","CH","DE","AT","LI")) %>% 
  st_transform(2154)

decoupe <- lissage_FR %>% 
  st_intersection(europe)

ggplot(data= decoupe)+
  geom_sf(aes(fill=taux_tr),color=NA)+
  geom_sf(data=regions,fill=NA,color="white")+
  scale_fill_viridis(name="Part des lieux de\nculte protestants\n(% lissé)",
                     option = "cividis",
                     direction = 1,
                     discrete=TRUE,
                     labels=c("[0,1]"="Moins d'1 %",
                              "(1,5]"="De 1 % à 5 %",
                              "(5,10]"="De 5 % à 10 %",
                              "(10,15]"="De 10 % à 15 %",
                              "(15,20]"="De 15 % à 20 %",
                              "(20,25]"="De 20 à 25 %",
                              "(25,50]"="De 25 % à 50 %",
                              "(50,75]"="De 50 % à 75 %"))+
  labs(title="Principaux territoires du protestantisme en France",
       subtitle="Présence de lieux de culte protestants en France métropolitaine",
       caption="Sources : contributions OpenStreetMap, \"place_of_worship\"\nTraitements et erreurs : Re_Mi_La\n ")+
  coord_sf(xlim=c(st_bbox(regions)$xmin,st_bbox(regions)$xmax),
           ylim=c(st_bbox(regions)$ymin,st_bbox(regions)$ymax))+
  theme_void()+
  theme(legend.position = "bottom",
        plot.title.position = "plot",
        text=element_text(family="Calibri",color="black"),
        plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(face="italic",size = 10),
        plot.background = element_rect(fill="white",color="white"))+
  guides(fill=guide_legend(nrow=4,bycol=TRUE))

ggsave("sorties/lieux_de_cultes.jpeg", 
       width = 1350,height = 1800, dpi=300,units = "px")
