library(osmdata)
library(tidyverse)
library(sf)
library(btb)


regions <- c("Occitanie",
             "Provence-Alpes-Côte d'Azur",
             "Nouvelle-Aquitaine",
             "Auvergne-Rhône-Alpes"),
"Bretagne",
"Normandie",
"Pays de la Loire",
"Centre-Val-de-Loire",
"Bourgogne-Franche-Comté",
"Grand Est",
"Ile-de-France",
"Hauts-de-France",
"Corse")


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
lieux_de_culte <- map_df(regions, cultes_par_regions)




lissage <- kernelSmoothing(dfObservations = lieux_de_culte,
                           sEPSG = 2154,
                           iCellSize = 1000,
                           iBandwidth = 50000)

regions <- st_read("donnees/REGION.shp") %>% 
  summarise()


lissage_decoupe <- st_intersection(lissage,reg)

ggplot(data= lissage %>% 
         mutate(taux=round(protestant/lieu*100,0)))+
  geom_sf(aes(fill=taux),color=NA)+
  scale_fill_viridis_b(name="Part des lieux de\ncultes protestants (%)")+
  labs(title="Le protestantisme en France : une histoire de bulles ?",
       subtitle="Présence de lieux de culte protestants en France métropolitaine",
       caption="Sources : contributions OpenStreetMap, \"place_of_worship\"\nTraitements et erreurs : Re_Mi_La")+
  theme_void()+
  theme(legend.position = "bottom",
        plot.title.position = "plot",
        text=element_text(family="Calibri",color="black"),
        plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=8),
        plot.caption = element_text(face="italic",size = 6),
        plot.background = element_rect(fill="white",color="white"))


ggsave("sorties/lieux_de_cultes.jpeg")
