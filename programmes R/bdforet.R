library(tidyverse)
library(sf)
library(extrafont)

loadfonts(dev="win")

bdforet <- st_read("donnees/BDFORET/DEP07.shp") %>% 
  bind_rows(st_read("donnees/BDFORET/DEP30.shp")) %>% 
  bind_rows(st_read("donnees/BDFORET/DEP48.shp")) %>% 
  bind_rows(st_read("donnees/BDFORET/DEP43.shp")) %>% 
  bind_rows(st_read("donnees/BDFORET/DEP26.shp")) %>% 
  bind_rows(st_read("donnees/BDFORET/DEP38.shp")) %>% 
  bind_rows(st_read("donnees/BDFORET/DEP42.shp")) %>% 
  filter(ESSENCE=="Châtaignier")

departement <- st_read("donnees/DEPARTEMENT.shp") %>% 
  filter(INSEE_DEP=="07")


foret_ard <-  st_read("donnees/BDFORET/DEP07.shp") %>% 
  select(ESSENCE)

surface <- foret_ard %>% 
  filter(ESSENCE=="Châtaignier") %>% 
  summarise() %>% 
  st_area()

surface2 <-foret_ard %>% 
  filter(ESSENCE!="Châtaignier") %>% 
  summarise() %>% 
  st_area()

surface/surface2*100
# bdforet %>% st_drop_geometry() %>% as.data.frame() %>%  select(ESSENCE) %>% distinct()

ggplot(data=bdforet)+
  geom_sf(aes(fill=(ESSENCE=="Châtaignier")),color=NA,show.legend = FALSE )+
  geom_sf(data=departement,color="forestgreen",fill=NA)+
  scale_fill_manual(name="",
                    values=c("TRUE"="forestgreen"),
                    labels=c("TRUE"="Forêt de châtaigniers"))+
  labs(title="8 %",
       subtitle="la part des châtaigneraies\ndans les formations végétales\nen Ardèche",
       caption="© IGN BD Forêt")+
  coord_sf(xlim = c(st_bbox(departement)$xmin, st_bbox(departement)$xmax),
           ylim = c(st_bbox(departement)$ymin, st_bbox(departement)$ymax), expand = TRUE) +
  theme_void()+
  theme(text = element_text(color="forestgreen"),
        plot.title = element_text(family="Calibri",size=50,face="bold"),
        plot.subtitle = element_text(size=8,family = "Courier New"),
        plot.caption = element_text(size=6,family="Calibri"),
        legend.position = "bottom",
        # panel.background = element_rect(fill="wheat1",color=NA),
        plot.background = element_rect(fill="wheat1",color="wheat1"))

ggsave("sorties/chataigneraies.jpeg")
