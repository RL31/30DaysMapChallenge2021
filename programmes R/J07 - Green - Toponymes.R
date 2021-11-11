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

extraction_vert <- function(FICHIER="cadastre-90-lieux_dits-shp.zip"){
  
  print(FICHIER)
  
  fichier_zip <- paste0("donnees/cadastre/",FICHIER)
  outDir<-"donnees/cadastre"
  unzip(fichier_zip,exdir=outDir)  
  
  dep <- st_read("donnees/cadastre/lieux_dits.shp", options="ENCODING=LATIN1") %>% 
    filter(str_detect(toupper(nom),"VERT"))
  
  file.remove("donnees/cadastre/lieux_dits.shp")
  
  return(dep)
  
}

ld_vert <- map_df(liste_fichiers,extraction_vert)
saveRDS(ld_vert,"donnees/ld_vert.RDS")

# ld_vert <- readRDS("donnees/ld_vert.RDS")

######################################
# Cartographie des zones plus rouges #
######################################

lieu_vert <- function(MOT="CROIX"){
  ld_ok <- ld_vert %>% 
    mutate(surface=as.numeric(st_area(.))/5000) %>% 
    st_centroid() %>%
    mutate(lieu=1,
           mot=if_else(str_detect(nom,MOT),1,0),
           x=st_coordinates(.)[,1],
           y=st_coordinates(.)[,2]) %>% 
    select(lieu,mot,surface,x,y) %>% 
    st_drop_geometry() %>%
    as.data.frame()
  
  lissage_vert <- kernelSmoothing(dfObservations = ld_ok,
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
  
  decoupe <- st_intersection(lissage_vert,silhouette)
  
  if(MOT=="BOIS|BOI|^BOI"){MOT2="BOIS"}
  else   if(MOT=="HYVERT|HIVERT"){MOT2="HYVERT"}
  else{ MOT2=MOT}
  carte <- ggplot(data = decoupe)+
    geom_sf(aes(fill=tr_lieu),color=NA, show.legend = FALSE)+
    annotate("text",label=str_to_title(MOT2),color="#005a32",x=180000,y=7000000,hjust=0,family="Calibri")+
    scale_fill_manual(values=c("1"='#edf8e9',
                               "2"='#c7e9c0',
                               "3"= '#a1d99b',
                               "4"='#74c476',
                               "5"= '#41ab5d',
                               "6"="#238b45",
                               "7"='#005a32'))+
    # labs(title=MOT)+
    theme_void()+
    theme(legend.position = "bottom",
          plot.title.position = "plot",
          text=element_text(family="Calibri",color="#005a32"),
          plot.title = element_text(face="bold",size=15),
          plot.subtitle = element_text(size=8),
          plot.caption = element_text(face="italic",size = 6),
          plot.background = element_rect(fill="white",color="white"))
  
  if(MOT=="BOIS|BOI|^BOI"){saveRDS(carte,paste0("sorties/carte_vert_","BOIS",".RDS"))}
  else  if(MOT=="HYVERT|HIVERT"){saveRDS(carte,paste0("sorties/carte_vert_","HYVERT",".RDS"))}
  else{ saveRDS(carte,paste0("sorties/carte_vert_",MOT,".RDS"))}
  
  # ggsave(paste0("sorties/",MOT,".jpg"))
}

liste_mots <- c("CHEMIN","CHENE","BOIS|BOI|^BOI","HYVERT|HIVERT","SAL","VILLE")

walk(liste_mots,lieu_vert)


ld_ok <- ld_vert %>% 
  mutate(surface=as.numeric(st_area(.))/5000) %>% 
  st_centroid() %>%
  mutate(lieu=1,
         x=st_coordinates(.)[,1],
         y=st_coordinates(.)[,2]) %>% 
  select(lieu,surface,x,y) %>% 
  st_drop_geometry() %>%
  as.data.frame()

lissage_vert <- kernelSmoothing(dfObservations = ld_ok,
                                 sEPSG = 2154,
                                 iCellSize = 5000, #2000
                                 iBandwidth = 40000) %>% 
  mutate(tr_lieu=cut(lieu,
                     breaks = c(0,.25,.5,.75,1,1.25,1.5,100),
                     include.lowest = TRUE,
                     labels = c("1","2","3","4","5","6","7"))) %>% 
  group_by(tr_lieu) %>% 
  summarise()

decoupe <- st_intersection(lissage_vert,silhouette)

carte <- ggplot(data = decoupe)+
  geom_sf(aes(fill=tr_lieu),color=NA, show.legend = FALSE)+
  annotate("text",label="Tous lieux-dits",color="#005a32",x=180000,y=7000000,hjust=0,family="Calibri")+
  scale_fill_manual(values=c("1"='#edf8e9',
                             "2"='#c7e9c0',
                             "3"= '#a1d99b',
                             "4"='#74c476',
                             "5"= '#41ab5d',
                             "6"="#238b45",
                             "7"='#005a32'))+
  labs(title="Noms de lieux : vert",
       subtitle = "Fréquence lissée des lieux-dits comprenant le terme \"vert\"")+
  theme_void()+
  theme(legend.position = "bottom",
        plot.title.position = "plot",
        text=element_text(family="Calibri",color="#005a32"),
        plot.title = element_text(face="bold",size=20),
        plot.subtitle = element_text(size=12),
        plot.caption = element_text(face="italic",size = 6),
        plot.background = element_rect(fill="white",color="white"))

saveRDS(carte,"sorties/carte_vert.RDS")

####################################
# Analyse des noms de lieux rouges #
####################################
mots_vert <- ld_vert %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  filter(!str_detect(toupper(nom),"COUVERT")) %>% 
  mutate(nettoyage= str_remove_all(str_replace_all(toupper(stringi::stri_trans_general(nom,"ASCII")),"-"," "),
                                   "AU |AUX |A |L'|LE |LA |LES |VERTE|VERT| VERTES| VERTS| VERTE| VERT|^VERTE |^VERT |^VERTES |^VERTS "),
         nettoyage= if_else(!nettoyage %in% c("BOIS","CAS","MAS", "CLOS"),str_replace_all(nettoyage,"S$",""),nettoyage ),
         nettoyage = str_remove_all(trimws(nettoyage,"both"),
                                    "^VERT |^VERTS ") ,
         nettoyage = if_else(str_detect(nettoyage,"MONT"),"MONT",nettoyage)) %>% 
  count(nettoyage) %>% 
  arrange(desc(n)) 


nuage <- mots_vert %>% 
  filter(! str_detect(nettoyage,"VERT")) %>%
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
  scale_color_gradient(low="#74c476",high="#005a32")+
  scale_size_area(max_size = 30)+
  labs(caption="Sources : DGFip, Cadastre\nTraitements et erreurs : Re_Mi_La\n ")+
  theme_void()+
  theme(legend.position = "bottom",
        plot.title.position = "plot",
        text=element_text(family="Calibri",color="#005a32"),
        plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=8),
        plot.caption = element_text(face="italic",size = 12),
        plot.background = element_rect(fill="white",color="white"))

saveRDS(nuage,"sorties/nuage_vert.RDS")

readRDS("sorties/carte_vert.RDS")+
  (readRDS("sorties/carte_vert_CHEMIN.RDS")+readRDS("sorties/carte_vert_CHENE.RDS"))/
  (readRDS("sorties/carte_vert_BOIS.RDS")+readRDS("sorties/carte_vert_HYVERT.RDS"))+
  readRDS("sorties/nuage_vert.RDS") +
  plot_layout() &
  theme(plot.background = element_rect(color  = 'white', fill ="white"))


ld_vert %>% st_drop_geometry() %>% as.data.frame() %>% 
  mutate(dpt=substr(commune,1,2)) %>% 
  filter(dpt %in% c("86")) %>% #,"86","49")) %>%
  view()
  count(dpt) %>% 
  view()

readRDS("sorties/carte_vert_CHEMIN.RDS")
readRDS("sorties/carte_vert_BOIS.RDS")
readRDS("sorties/carte_vert_CHENE.RDS")
readRDS("sorties/carte_vert_HYVERT.RDS")
readRDS("sorties/carte_vert_SAL.RDS")

readRDS("sorties/carte_vert_VILLE.RDS")



ggsave(filename="sorties/montage_vert.jpg")

