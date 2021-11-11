library(tidyverse)
library(sf)
library(extrafont)

loadfonts(dev="win")

manifs <- rgdal::readOGR("donnees/17novembre2018.kml", "17 novembre 2018")%>% 
  st_as_sf(crs=4326) %>% 
  st_transform(crs=2154)

ndv <- read.csv("donnees/FILO2018_DISP_COM.csv",sep=";") %>% 
  select(CODGEO,Q218)

ndv_communes <- st_read("donnees/COMMUNE.shp") %>% 
  left_join(ndv,by=c("INSEE_COM"="CODGEO")) %>% 
  mutate(tr_ndv=cut(Q218,breaks = quantile(Q218,probs=c(0,0.33,0.66,1),na.rm=TRUE),
                    labels=c(1,2,3)))

croisement <- st_intersection(manifs, ndv_communes %>% select(Q218,tr_ndv))

croisement %>%
  filter(!is.na(tr_ndv)) %>% 
  ggplot()+
  geom_sf(aes(color=tr_ndv),alpha=.9)+
  scale_color_manual(name="Niveau de vie médian\ndes habitants\nde la commune",
                     values=c("1"="gold","2"="gray40","3"="gold4"),
                     labels=c("1"="Faible (n = 431)","2"="Moyen (n = 196)","3"="Elevé (n = 149)"))+
  labs(title="Points jaunes et ronds-points :\noù sont les gilets jaunes ?",
       subtitle="Lieux des manifestations le 17 novembre 2018",
       caption="Sources : Mouvement des gilets jaunes, carte officielle\nInsee, Fichier localisé social et fiscal 2018\nIGN, Admin Express COG\nTraitements et erreurs : Re_Mi_La")+
  theme_void()+
  theme(legend.position = "left",
        plot.title.position = "plot",
        text=element_text(family="Calibri",color="gold"),
        plot.title = element_text(face="bold",size=20),
        plot.caption = element_text(face="italic",size = 8),
        plot.background = element_rect(fill="black",color="black"))+
  guides(color=guide_legend(override.aes = list(size = 5)))

ggsave("sorties/giletsjaunes.jpeg")
