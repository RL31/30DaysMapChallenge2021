library(tidyverse)
library(data.table)
library(sf)

fermes <- read.delim("donnees/coord_google_fermes.txt",sep=",") %>%
  group_by(territoire) %>% 
  mutate(poi=paste0(territoire,row_number())) %>% 
  ungroup() %>% 
  st_as_sf(coords=c("x","y")) %>% 
  st_set_crs(4326) %>% 
  st_transform(2154)

#length(couleurs_briques)
fermes <- fermes %>%
  bind_cols(fermes %>% st_coordinates() %>% as.data.frame()) %>%
  st_drop_geometry() %>% 
  rename(y=Y,x=X) %>% 
  tibble() %>% 
  filter(territoire!="Dombes")


COMM_FRMET_2020 <- st_read("donnees/COMMUNE.shp") %>% 
  st_transform(2154) %>% 
  select(INSEE_COM,geometry)

# fonction pour cr?er les cartes
crea_carto_cadastre <- function(poi, x ,y , rayon = 25 ){
  
  # cr?ation des sous dossiers 'data' et 'img'
  # dir.create(file.path('./donnees/cadastre/', 'data'), showWarnings = FALSE)
  dir.create(file.path('./donnees/cadastre/', 'img'), showWarnings = FALSE)
  
  # emprise ? cartographier
  bbox_ADHOC <- data.frame(
    x =  x, 
    y = y ) %>%
    st_as_sf(
      coords = c("x", "y"),
      crs = 2154, remove = FALSE) %>%
    st_buffer(dist = rayon) 
  
  # liste des communes concern?es
  bbox_ADHOC_comm <-
    st_join( bbox_ADHOC,COMM_FRMET_2020 ) %>%
    st_drop_geometry() %>%
    select(INSEE_COM) %>%
    as.vector() %>%
    pull()
  
  # listes des d?partements concern?s
  bbox_ADHOC_dep <-
    bbox_ADHOC_comm %>%
    str_sub(., 1, 2) %>%
    unique()
  
  bbox_ADHOC <- bbox_ADHOC %>%
    st_bbox() 
  
  # d?compression des shps
  f_dezip <- function(dep, fichier){
    unzip(paste0("./donnees/cadastre/cadastre-",dep,"-",fichier,"-shp.zip"),
          exdir=paste0("./donnees/cadastre/cadastre-",dep))
  }
  
  # map2(bbox_ADHOC_dep, "parcelles", f_dezip)
  map2(bbox_ADHOC_dep, "batiments", f_dezip)
  
  # import des shapefiles batiments et parcelles
  f_import_shp <- function(dep, type) {
    st_read(paste0('./donnees/cadastre/cadastre-',dep,'/',type,'.shp'))
  }
  
  CADASTRE_BATIMENTS <- map2_dfr(bbox_ADHOC_dep,"batiments", f_import_shp) 
  # CADASTRE_PARCELLES <- map2_dfr(bbox_ADHOC_dep,"parcelles", f_import_shp) 
  
  # suppression du dossier contenant les shapes d?zipp?s
  f_suppr_zip <- function(dep) {
    unlink(paste0('./donnees/cadastre/cadastre-',dep),
           recursive=TRUE)
  }
  
  map(bbox_ADHOC_dep, f_suppr_zip)
  
  #### cartographies
  
  # carte 1?re version
  carto_v1 <- 
    ggplot() +
    # geom_sf(data = CADASTRE_PARCELLES %>%
    #           filter(commune %in% bbox_ADHOC_comm) %>%
    #           identity(),
    #         color = col_1,
    #         fill = "white",
    #         size = 0.6) +
    geom_sf(data = CADASTRE_BATIMENTS %>%
              filter(commune %in% bbox_ADHOC_comm) %>%
              identity(),
            fill = "black",
            color = NA) +
    scale_x_continuous(name = "", limits = c(bbox_ADHOC$xmin %>% as.vector(), bbox_ADHOC$xmax %>% as.vector())) +
    scale_y_continuous(name = "", limits = c(bbox_ADHOC$ymin %>% as.vector(), bbox_ADHOC$ymax %>% as.vector())) +
    theme(panel.background = element_rect(fill = "white")) +
    coord_sf(datum = NA) 
  
  # export png
  ggsave(
    paste0("./donnees/cadastre/img/carte_cadastre_",poi,"_v1.png"),
    plot = carto_v1, 
    height = 14, width = 14,
    dpi = 600)
  saveRDS(carto_v1,paste0("./donnees/cadastre/img/carte_cadastre_",poi,"_v1.rds"))
  
  
}



pmap(list(fermes$poi, 
          fermes$x, 
          fermes$y,
          30),
     crea_carto_cadastre)




library(patchwork)
lyonnais 1 3 4 6 7 9 10
lauragais 1 4 5 6 7 8 9 10

ly1 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lyonnais1_v1.rds")
# ly2 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lyonnais2_v1.rds")
ly3 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lyonnais3_v1.rds")
ly4 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lyonnais4_v1.rds")
# ly5 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lyonnais5_v1.rds")
ly6 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lyonnais6_v1.rds")
# ly7 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lyonnais7_v1.rds")
# ly8 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lyonnais8_v1.rds")
ly9 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lyonnais9_v1.rds")
ly10 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lyonnais10_v1.rds")
ly11 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lyonnais11_v1.rds")
# ly12 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lyonnais12_v1.rds")
ly13 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lyonnais13_v1.rds")
ly14 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lyonnais14_v1.rds")
ly15 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lyonnais15_v1.rds")
ly16 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lyonnais16_v1.rds")
ly17 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lyonnais17_v1.rds")


lau1 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lauragais1_v1.rds")
#lau2 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lauragais2_v1.rds")
#lau3 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lauragais3_v1.rds")
lau4 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lauragais4_v1.rds")
lau5 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lauragais5_v1.rds")
lau6 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lauragais6_v1.rds")
#lau7 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lauragais7_v1.rds")
#lau8 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lauragais8_v1.rds")
lau9 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lauragais9_v1.rds")
lau10 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lauragais10_v1.rds")
lau11 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lauragais11_v1.rds")
lau12 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lauragais12_v1.rds")
lau13 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lauragais13_v1.rds")
lau14 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lauragais14_v1.rds")
lau15 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lauragais15_v1.rds")
lau16 <-  readRDS("donnees/cadastre/img/carte_cadastre_Lauragais16_v1.rds")


library(cowplot)
plot_row <- plot_grid(lau1,lau4,lau5,lau6,lau9,lau10,lau11,lau12,lau13,lau14,lau15,lau16,,ncol=4 )

# now add the title
title <- ggdraw() + 
  draw_label(
    "Ancien habitat rural isolé : s'adapter au vent et au froid",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme()
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)




((ly1+ly4)/(ly6+ly3)/(ly9+ly10))+((lau1+lau3)/(lau4+lau6)/(lau7+lau9))
