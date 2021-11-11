library(tidyverse)
library(sf)
library(extrafont)
library(cowplot)

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
# 
# surface <- foret_ard %>% 
#   filter(ESSENCE=="Châtaignier") %>% 
#   summarise() %>% 
#   st_area()
# 
# surface2 <-foret_ard %>% 
#   filter(ESSENCE!="Châtaignier") %>% 
#   summarise() %>% 
#   st_area()
# 
# surface/surface2*100

carte_chataigneraie <- ggplot(data=bdforet)+
  geom_sf(aes(fill=(ESSENCE=="Châtaignier")),color=NA,show.legend = FALSE )+
  geom_sf(data=departement,color="forestgreen",fill=NA)+
  scale_fill_manual(name="",
                    values=c("TRUE"="forestgreen"),
                    labels=c("TRUE"="Forêt de châtaigniers"))+
  
  annotate("text",x=760000 ,y=6460000,label="8%",size=20,color="forestgreen",hjust=0)+
  annotate("text",x=760000 ,y=6440000,label="des formations végétales\nsont des châtaigneraies\nen Ardèche",
           size=2.5,color="forestgreen",hjust=0)+
  labs(caption="Sources : IGN, BD Forêt\nTraitements et erreurs : Re_Mi_La\n ")+
  coord_sf(xlim = c(763000, st_bbox(departement)$xmax),
           ylim = c(st_bbox(departement)$ymin, st_bbox(departement)$ymax), expand = TRUE) +
  theme_void()+
  theme(text = element_text(color="forestgreen"),
        plot.title = element_text(family="Calibri",size=50,face="bold"),
        plot.subtitle = element_text(size=8,family = "Courier New"),
        plot.caption = element_text(size=6,family="Calibri"),
        legend.position = "bottom",
        plot.background = element_rect(fill="wheat1",color="wheat1"))

carte_fr <- st_read("donnees/DEPARTEMENT.shp") %>% 
  ggplot()+
  geom_sf(aes(fill=INSEE_DEP=="07"),color="white",show.legend = FALSE )+
  scale_fill_manual(values = c("TRUE"="forestgreen","FALSE"="white"))+
  theme_void()+
  theme(plot.background = element_rect(fill="wheat1",color="wheat1"))

plot.with.inset <-ggdraw() +
  theme(plot.background = element_rect(fill="wheat1",color="wheat1"))+
  draw_plot(carte_chataigneraie,x=0,width = 1) +
  draw_plot(carte_fr, x = 0.8, y = .05, width = .2, height = .2)


ggsave(plot=plot.with.inset,"sorties/chataigneraies.jpeg",
       width=3.8, height = 4.84)
