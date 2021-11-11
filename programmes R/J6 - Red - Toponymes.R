library(tidyverse)
library(sf)
library(btb)
library(ggwordcloud)
#library(wordcloud2)
library(extrafont)
library(patchwork)
# devtools::install_github("lchiffon/wordcloud2")

loadfonts(dev="win")

#################################
# Fond de carte de la métropole #
#################################

silhouette <- st_read("donnees/REGION.shp") %>% 
  summarise()
# ggplot(data=silhouette)+
#   geom_sf(fill="black",color="black")+
#   theme_void()
# ggsave("sorties/silhouette.png")

####################################
# Extraction des lieux-dits rouges #
####################################
liste_fichiers <-  list.files(path="donnees/cadastre/",
                              pattern="lieux_dits-shp.zip")

extraction_rouge <- function(FICHIER="cadastre-90-lieux_dits-shp.zip"){
  
  print(FICHIER)
  
  fichier_zip <- paste0("donnees/cadastre/",FICHIER)
  outDir<-"donnees/cadastre"
  unzip(fichier_zip,exdir=outDir)  
  
  dep <- st_read("donnees/cadastre/lieux_dits.shp", options="ENCODING=LATIN1") %>% 
    filter(str_detect(nom,"ROUGE|rouge"))
  
  file.remove("donnees/cadastre/lieux_dits.shp")
  
  return(dep)
  
}

# ld_rouge <- map_df(liste_fichiers,extraction_rouge)
# saveRDS(ld_rouge,"donnees/ld_rouge.RDS")

ld_rouge <- readRDS("donnees/ld_rouge.RDS")

######################################
# Cartographie des zones plus rouges #
######################################

lieu_rouge <- function(MOT="CROIX"){
  ld_ok <- ld_rouge %>% 
    mutate(surface=as.numeric(st_area(.))/5000) %>% 
    st_centroid() %>%
    mutate(lieu=1,
           mot=if_else(str_detect(nom,MOT),1,0),
           x=st_coordinates(.)[,1],
           y=st_coordinates(.)[,2]) %>% 
    select(lieu,mot,surface,x,y) %>% 
    st_drop_geometry() %>%
    as.data.frame()
  
  lissage_rouge <- kernelSmoothing(dfObservations = ld_ok,
                                   sEPSG = 2154,
                                   iCellSize = 5000, #2000
                                   iBandwidth = 40000) %>% 
    mutate(tr_lieu=cut(mot,#/lieu*100,
                       #breaks = c(0,17,33,50,66,83,100),
                       breaks = c(0,.1,.2,.3,.4,.5,1,100),
                       include.lowest = TRUE,
                       labels = c("1","2","3","4","5","6","7"))) %>% 
    group_by(tr_lieu) %>% 
    summarise()
  
  decoupe <- st_intersection(lissage_rouge,silhouette)
  
  carte <- ggplot(data = decoupe)+
    geom_sf(aes(fill=tr_lieu),color=NA, show.legend = FALSE)+
    annotate("text",label=str_to_title(MOT),color="#cb181d",x=180000,y=7000000,hjust=0,family="Calibri")+
    scale_fill_manual(values=c("1"='#fcbba1',
                               "2"='#fc9272',
                               "3"= '#fb6a4a',
                               "4"='#ef3b2c',
                               "5"= '#cb181d',
                               "6"="#a50f15",
                               "7"='#67000d'))+
    # labs(title=MOT)+
    theme_void()+
    theme(legend.position = "bottom",
          plot.title.position = "plot",
          text=element_text(family="Calibri",color="#cb181d"),
          plot.title = element_text(face="bold",size=15),
          plot.subtitle = element_text(size=8),
          plot.caption = element_text(face="italic",size = 6),
          plot.background = element_rect(fill="cornsilk",color="cornsilk"))
  
  saveRDS(carte,paste0("sorties/carte",MOT,".RDS"))
  # ggsave(paste0("sorties/",MOT,".jpg"))
}

liste_mots <- c("TERRE","CROIX","CHAMP","MAISON","MONT")

walk(liste_mots,lieu_rouge)


ld_ok <- ld_rouge %>% 
  mutate(surface=as.numeric(st_area(.))/5000) %>% 
  st_centroid() %>%
  mutate(lieu=1,
         x=st_coordinates(.)[,1],
         y=st_coordinates(.)[,2]) %>% 
  select(lieu,surface,x,y) %>% 
  st_drop_geometry() %>%
  as.data.frame()

lissage_rouge <- kernelSmoothing(dfObservations = ld_ok,
                                 sEPSG = 2154,
                                 iCellSize = 5000, #2000
                                 iBandwidth = 40000) %>% 
  mutate(tr_lieu=cut(lieu,
                     breaks = c(0,.25,.5,.75,1,1.25,1.5,100),
                     include.lowest = TRUE,
                     labels = c("1","2","3","4","5","6","7"))) %>% 
  group_by(tr_lieu) %>% 
  summarise()

decoupe <- st_intersection(lissage_rouge,silhouette)

carte <- ggplot(data = decoupe)+
  geom_sf(aes(fill=tr_lieu),color=NA, show.legend = FALSE)+
  annotate("text",label="Tous lieux-dits",color="#cb181d",x=180000,y=7000000,hjust=0,family="Calibri")+
  scale_fill_manual(values=c("1"='#fcbba1',
                             "2"='#fc9272',
                             "3"= '#fb6a4a',
                             "4"='#ef3b2c',
                             "5"= '#cb181d',
                             "6"="#a50f15",
                             "7"='#67000d'))+
  labs(title="Noms de lieux : rouge",
       subtitle = "Fréquence lissée des lieux-dits comprenant le terme \"rouge\"")+
  theme_void()+
  theme(legend.position = "bottom",
        plot.title.position = "plot",
        text=element_text(family="Calibri",color="#cb181d"),
        plot.title = element_text(face="bold",size=20),
        plot.subtitle = element_text(size=12),
        plot.caption = element_text(face="italic",size = 6),
        plot.background = element_rect(fill="cornsilk",color="cornsilk"))

saveRDS(carte,"sorties/carte_rouge.RDS")

####################################
# Analyse des noms de lieux rouges #
####################################
mots_rouge <- ld_rouge %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  mutate(nettoyage= str_remove_all(str_replace_all(nom,"-"," "),
                                   "AU |AUX |A |LE |LA |LES | ROUGES| ROUGE|^ROUGE |^ROUGES "),
         nettoyage= if_else(!nettoyage %in% c("BOIS","CAS","MAS", "CLOS"),str_replace_all(nettoyage,"S$",""),nettoyage ),
         nettoyage = str_remove_all(trimws(nettoyage,"both"),
                                    "^ROUGE |^ROUGES ") ,
         nettoyage = if_else(str_detect(nettoyage,"MONT"),"MONT",nettoyage)) %>% 
  count(nettoyage) %>% 
  arrange(desc(n)) 


nuage <- mots_rouge %>% 
  filter(! str_detect(nettoyage,"ROUGE")) %>%
  head(1000) %>% 
  mutate(n=log(n),#/909,#log(n)^2,
         alea=round(runif(1000),1),
         angle=round(runif(1000),0)*90) %>% 
  ggplot(aes(label = nettoyage , size=n)) +
  geom_text_wordcloud_area(mask=png::readPNG("sorties/silhouette.png"),
                           aes(color=alea,angle=angle,family="Calibri" ),
                           grid_size = 2,
                           grid_margin = 0,
                           max_grid_size = 128,
                           rm_outside = TRUE)+
  scale_color_gradient(low="#fcbba1",high="#cb181d")+
  scale_size_area(max_size = 20)+
  labs(caption="Sources : DGFip, Cadastre\nTraitements et erreurs : Re_Mi_La\n ")+
  theme_void()+
  theme(legend.position = "bottom",
        plot.title.position = "plot",
        text=element_text(family="Calibri",color="#cb181d"),
        plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=8),
        plot.caption = element_text(face="italic",size = 12),
        plot.background = element_rect(fill="cornsilk",color="cornsilk"))

saveRDS(nuage,"sorties/nuage_rouge.RDS")

readRDS("sorties/carte_rouge.RDS")+
  (readRDS("sorties/carteCROIX.RDS")+readRDS("sorties/carteTERRE.RDS"))/
  (readRDS("sorties/carteCHAMP.RDS")+readRDS("sorties/carteMAISON.RDS"))+
  readRDS("sorties/nuage_rouge.RDS") +
  plot_layout() &
  theme(plot.background = element_rect(color  = 'cornsilk', fill ="cornsilk"))


ggsave(filename="sorties/montage_rouge2.jpg")

