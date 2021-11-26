library(tidyverse)
library(sf)


base_propre <- readRDS("donnees/base_alertes_propres.RDS")

dep <- st_read("donnees/DEPARTEMENT.shp")
# base_propre %>% count(alerte) %>% view()

base_carto <- base_propre %>%
mutate(pluie_inondation = str_detect(tolower(alerte),"pluie-inondation"),
       canicule = str_detect(tolower(alerte),"canicule"),
       vagues_submersion = str_detect(tolower(alerte),"submersion"),
       grand_froid = str_detect(tolower(alerte),"grand froid"),
       neige_verglas = str_detect(tolower(alerte),"neige-verglas"),
       orages = str_detect(tolower(alerte),"orages"),
       avalanches = str_detect(tolower(alerte),"avalanche"),
       vent = str_detect(tolower(alerte),"vent")) %>%
group_by(dpt) %>% 
  summarise(n_inondation=sum(pluie_inondation),
            n_canicule=sum(canicule),
            n_submersion=sum(vagues_submersion),
            n_froid=sum(grand_froid),
            n_neige=sum(neige_verglas),
            n_orage=sum(orages),
            n_avalanche=sum(avalanches),
            n_vent=sum(vent)) %>% 
  pivot_longer(cols=starts_with("n"),names_to="risque",values_to="frequence") %>% 
  mutate(tranche= cut(frequence,
                      breaks =quantile(frequence, probs = c(0,.33,.66,1)),
                      include.lowest = TRUE ),
         risque=str_to_title(substr(risque,3,nchar(risque)))) %>% 
  left_join(dep %>% 
              mutate(NOM= case_when(str_detect(NOM,"Indre") & str_detect(NOM,"Loire") ~ "Indre-et-Loire",
                                    str_detect(NOM,"Armor") ~ "Côtes-d'Armor",
                                    TRUE ~ NOM)),
            by=c("dpt"="NOM")) %>% 
    st_as_sf()

ggplot()+
  geom_sf(data=base_carto ,
          aes(fill= frequence),color="NA")+
  scale_fill_viridis_b()+
  facet_wrap(~risque )+
  labs(title="Avalanches dans les Pyrénées, canicules dans le centre-est,\nsubmersions sur la façade Atlantique : quelles alertes météo en métropole ?",
       subtitle="Fréquence des alertes météo depuis 2012, par département\n  \n  ",
       caption="Sources : Météo-France\nTraitements et erreurs : Re_Mi_La\n",
       fill="Nombre de jours\nd'alerte depuis 2012")+
  theme_void()+
  theme(legend.position = "bottom",
        plot.caption = element_text(face="italic"),
        plot.background = element_rect(fill="white",color="white"))
ggsave("sorties/alertes_meteo2.jpg",width = 20,height = 20,units = "cm"  )


# faire répartition annuelle moyenne