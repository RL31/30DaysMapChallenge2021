library(tidyverse)
library(sf)
library(btb)
library(ggwordcloud)
#library(wordcloud2)
library(extrafont)
# devtools::install_github("lchiffon/wordcloud2")

loadfonts(dev="win")

# Extraction des lieux-dits
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

# Cartographie
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
  scale_fill_manual(values=c("1"='#fcbba1',
                             "2"='#fc9272',
                              "3"= '#fb6a4a',
                             "4"='#ef3b2c',
                              "5"= '#cb181d',
                             "6"="#a50f15",
                             "7"='#67000d'))+
  theme_void()+
  theme(legend.position = "bottom",
        plot.title.position = "plot",
        text=element_text(family="Calibri",color="#cb181d"),
        plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=8),
        plot.caption = element_text(face="italic",size = 6),
        plot.background = element_rect(fill="white",color="white"))




# Nuage de mots

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


nuage <- mots_rouge %>% filter(! str_detect(nettoyage,"ROUGE")) %>%
  head(100) %>% 
      mutate(n=n/909,#log(n)^2,
         alea=round(runif(100),1),
         angle=round(runif(100),0)*90) %>% 
ggplot(aes(label = nettoyage , size=n)) +
  geom_text_wordcloud_area(mask=png::readPNG("sorties/silhouette.png"),
                           aes(color=alea,angle=angle,family="Calibri" ),
                           grid_size = 1,
                           max_grid_size = 3,
                           rm_outside = TRUE)+
  scale_color_gradient(low="#fcbba1",high="#67000d")+
  scale_size_area(max_size = 100)+
   labs(title="La vie en rouge",
        subtitle="Lieux-dits de métropole comprenant le mot \"rouge\"\nCarte de répartition lissée et nuage des mots associés",
        caption="Sources : DGFip, Cadastre\nTraitements et erreurs : Re_Mi_La\n ")+
    theme_void()+
  theme(legend.position = "bottom",
        plot.title.position = "plot",
        text=element_text(family="Calibri",color="#cb181d"),
        plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=8),
        plot.caption = element_text(face="italic",size = 6),
        plot.background = element_rect(fill="white",color="white"))

ggsave(plot=nuage,filename="sorties/nuage.png")

silhouette <- st_read("donnees/REGION.shp") %>% 
  summarise()
# ggplot(data=silhouette)+
#   geom_sf(fill="black",color="black")+
#   theme_void()
# ggsave("sorties/silhouette.png")


# 
# colorVec = rep(c('#67000d','#a50f15','#cb181d','#ef3b2c','#fb6a4a','#fc9272','#fcbba1'), length.out=nrow(mots_rouge))
# 
# wordcloud2(mots_rouge %>% filter(! str_detect(nettoyage,"ROUGE")) %>% mutate(n=log(n)^2),
#            fontFamily = "Calibri",
#            size = 1, minRotation = -pi/2, maxRotation = -pi/2,
#           # rotateRatio = 1,
#          color = colorVec,
#           figPath = "sorties/silhouette.png" )#c('#67000d','#a50f15','#cb181d','#ef3b2c','#fb6a4a','#fc9272','#fcbba1'))#,'#fee0d2','#fff5f0'))



library(cowplot)
plot.with.inset <-ggdraw() +
  theme(plot.background = element_rect(fill="white",color="white"))+
  draw_plot(nuage,x=0,width = 1) +
  draw_plot(carte, x = 0, y = .05, width = .2, height = .2)
