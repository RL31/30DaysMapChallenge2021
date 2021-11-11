library(sf)
library(tidyverse)
library(btb)
library(classInt)
library(hexbin)
library(extrafont)
library(patchwork)  

loadfonts(dev="win")

# de la bd topo on ne conserve que la variable MAT_TOITS et on passe tout en st_centroid()
pour_lissage <- readRDS("donnees/pour_lissage.RDS")

pour_lissage <- pour_lissage%>% 
  mutate(x = unlist(map(pour_lissage$geometry,1)),
         y = unlist(map(pour_lissage$geometry,2)),
         ardoises=if_else(substr(str_replace_na(MAT_TOITS,"X"),1,1)=="2",1,0),
         briques=if_else(substr(str_replace_na(MAT_MURS,"X"),1,1)=="4",1,0),
         batiment=1) %>% 
  select(x,y,ardoises,briques,batiment)

lissage <- kernelSmoothing(pour_lissage %>% filter(!is.na(x) & !is.na(y)),
                           sEPSG = 2154,
                           iCellSize = 500,
                           iBandwidth = 10000)

lissage <- lissage %>% 
  mutate(taux=ardoises/batiment*100,
         tranche=cut(taux,
                     breaks=c(0,5,100),
                     labels=c("Autres","Ardoises"),
                     include.lowest=TRUE))


toits <- pour_lissage %>%
  ggplot(aes(x, y))+
  stat_summary_hex(bins=20,na.rm=FALSE, aes(z = ardoises),
                   fun = ~mean(.x)*100, colour="white")+
  theme_void()+
  theme(legend.position = "bottom",
        text=element_text(family="Calibri",color="black"),
        plot.title = element_text(face="bold",size=20,color="grey10",hjust=1),
        plot.subtitle = element_text(size=8),
        plot.caption = element_text(face="italic",size = 6),
        plot.background = element_rect(fill="white",color="white"))+
  
  scale_fill_gradient(name="Toitures\nen ardoises (%)\n",
                      low="wheat",high="grey10")+
  labs(title = "Toits en ardoise...",
       subtitle = " \n ")

murs <- pour_lissage %>%
  ggplot(aes(x, y))+
  stat_summary_2d(bins=20,na.rm=FALSE, aes(z = briques),
                  fun = ~mean(.x)*100, colour="white")+
  theme_void()+
  theme(legend.position = "bottom",
        text=element_text(family="Calibri",color="black"),
        plot.title = element_text(face="bold",size=20,color="sienna2"),
        plot.subtitle = element_text(size=8),
        plot.caption = element_text(face="italic",size = 10),
        plot.background = element_rect(fill="white",color="white"))+  scale_fill_gradient(name="Murs\nen briques (%)\n",
                                                                                          low="grey90",high="sienna2")+
  labs( title="et murs en briques d'Occitanie",
        caption="Sources : IGN, BD-Topo\nTraitements et erreurs : Re_Mi_La\n ")

toits+murs


#ggsave(toits+murs,"sorties/toits_et_murs.jpg")
