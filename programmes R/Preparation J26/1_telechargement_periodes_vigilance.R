library(tidyverse)

liste_dates <- tibble(annee=rep(seq(2012,2021,1),each=372),
                      mois=rep(rep(seq(1,12,1),each=31),times=10),
                      jour=rep(seq(from=1,to=31,by=1),times=120)) %>% 
  mutate(date=paste0(annee,"-",str_pad(mois,2,"left","0"),"-",str_pad(jour,2,"left","0"))) %>% 
  select(date) %>%
 pull()



telechargement_vigilances <- function(date="2015-07-01"){
  
  lien <- paste0("http://vigilance-public.meteo.fr/telechargement.php?dateVigi=",date,"&base=vigilance4")
  
  download.file(lien,"xml/temp.zip",method = "curl")
  
  fichier_zip <- "xml/temp.zip"
  outDir<-"xml"
  unzip(fichier_zip,exdir=outDir )  
  
  file.remove(list.files(pattern = ".gif", recursive=TRUE))
  
}

walk(liste_dates,telechargement_vigilances)


