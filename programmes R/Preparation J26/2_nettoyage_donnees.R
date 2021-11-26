library(tidyverse)
library(xml2)

liste_xml <- list.files(path="xml/",
                        pattern=".xml")

liste_dpts <- read.delim("depts2012.txt") %>% 
  select(NCCENR)

liste_reg <- read.delim("reg2012.txt") %>% 
  select(NCC,REGION) %>%
  left_join(read.delim("depts2012.txt") %>% select(REGION,NCCENR),by="REGION") %>% 
  group_by(NCC) %>% 
  mutate(nom=paste0("dpt",row_number())) %>% 
  pivot_wider(id_cols="NCC",names_from="nom",values_from="NCCENR") %>% 
  mutate(composition=paste0(dpt1,", ",dpt2 )) %>% 
  select(NCC)

liste_geo <- liste_dpts %>% 
  bind_rows(liste_reg) %>% 
  as.matrix() %>% 
  as.list() %>% 
  paste(.,collapse = "|")

extraction_xml <- function(fichier){
  
  #  fichier <- "2012_07_05-19_00_CMIRN.xml"
  data <- read_xml(paste0("xml/",fichier)) #2015_07_01-16_17_CNP
  
  print(fichier)
  phenomene <- xml_attr(xml_find_first(data, ".//Phenomenes"),"evenement") %>% # first puis all
    #  str_extract_all("[:alpha:]+") %>% 
    as.character()
  
  suivis <- xml_text(xml_find_all(data, ".//Descriptif/Titre/Paragraphe")) %>% 
    as.data.frame() %>% 
    set_names("texte") %>% 
    separate(col=texte,sep=":",into=c("a","b"),extra = "drop") %>% 
    filter(str_detect(a,"suivi")) %>% 
    mutate(a=stringi::stri_trans_general(as.character(str_extract(a,"[:alpha:]+")),"latin-ascii")) %>% 
    pivot_wider(names_from="a",values_from="b") %>% 
    mutate(date=substr(fichier,1,16),
           alerte=phenomene,
           across(c("Debut","Maintien","Fin"),
                  ~  map_chr(str_extract_all(string=.,pattern=liste_geo),
                             ~ str_c(.x, collapse=", ") )
           )
    ) %>% 
    select(date, alerte,Debut,Maintien,Fin) 
  
  return(suivis)
  
}

base_brute <- map_df(liste_xml,extraction_xml)

base_intermediaire <- base_brute %>% 
  mutate(date_ok=lubridate::ymd(str_replace_all(substr(date,1,10),"_","-")),
         dpt_concerne = str_replace_all(paste0(Debut,", ",Maintien),pattern="^, ","")) %>% 
  filter(dpt_concerne!="") %>% 
  select(date_ok, alerte, dpt_concerne) %>% 
  distinct() %>% 
  bind_cols(str_split_fixed(string=.$dpt_concerne,pattern=", ",n=Inf) %>% 
              as.data.frame()) %>% 
  select(-dpt_concerne) %>% 
  pivot_longer(cols=-c("date_ok", "alerte"), names_to="asuppr", values_to="dpt") %>% 
  filter(dpt!="") %>% 
  select(-asuppr) %>% 
  distinct()

base_dep <- base_intermediaire %>% 
  filter(!trimws(dpt) %in% (liste_reg %>% pull()) )

base_reg <- base_intermediaire %>% 
  filter(trimws(dpt) %in% (liste_reg %>% pull()) ) %>% 
  left_join(read.delim("reg2012.txt") %>% 
              select(NCC,REGION) %>%
              left_join(read.delim("depts2012.txt") %>% select(REGION,NCCENR),by="REGION") %>% 
              group_by(NCC) %>% 
              mutate(nom=paste0("dpt",row_number())) %>% 
              pivot_wider(id_cols="NCC",names_from="nom",values_from="NCCENR"),
            by=c("dpt"="NCC")) %>% 
  select(-dpt) %>% 
  pivot_longer(cols=-c("date_ok", "alerte"), names_to="asuppr", values_to="dpt") %>% 
  filter(!is.na(dpt)) %>% 
  select(-asuppr) %>% 
  distinct()

base_propre <- base_dep %>% 
  bind_rows(base_reg)

saveRDS(base_propre,"donnees/base_alertes_propres.RDS")
