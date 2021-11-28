library(tidyverse)
library(sf)
library(geosphere) # pour le calcul de l'azimuth
library(nngeo) # pour st_segment
library(spatstat)

# BD TOPO
bdtopo <- st_read("donnees/BDTOPO/BATIMENT.shp") %>% 
  select(NATURE,geometry,ID,USAGE1)

segments <- bdtopo %>% 
  filter(NATURE=="Serre" ) %>% 
  st_segments()

bdtopo %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  count(NATURE) %>% 
    slice_max(order_by = n, n=20)


angle_le_plus_long <- segments %>% 
  st_transform(4326) %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  group_by(L1) %>% 
  mutate(id_segment=order(X) )%>% 
  ungroup() %>% 
  pivot_wider(id_cols = L1,names_from=id_segment,values_from=c(X,Y)) %>% 
  mutate(azimuth=round(bearing( .[,c("X_1","Y_1")],.[,c("X_2","Y_2")])/10,0)*10) %>% 
  bind_cols(segments %>% 
              mutate(longueur=st_length(.)) %>% 
              st_drop_geometry() %>% 
              as.data.frame() %>% 
              select(ID, longueur)) %>% 
  group_by(ID,azimuth) %>% 
  summarise(longueur_totale=sum(longueur)) %>% 
  group_by(ID) %>% 
  mutate(max=max(longueur_totale)) %>% 
  filter(longueur_totale==max)


ggplot(angle_le_plus_long,
       aes(x = azimuth,
           weight = longueur_totale)) +
  geom_histogram(binwidth = 10, boundary = 5,
                 size = .01, closed = "left",
                 fill = SIGN_GREEN,
                 colour = "gray10") +
  scale_x_continuous(breaks = c(0, 90, 180, 270),
                     limits = c(-5, 355),
                     labels = c("N", "E", "S", "W")) +
  coord_polar(start = -pi/36) +
  xlab(NULL) + ylab(NULL) +
  theme_bw(base_family = PLOT_FONT) +
  theme(plot.title = element_text(family = PLOT_FONT, hjust = 0.5,
                                  size = 32,
                                  colour = SIGN_GREEN),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(colour = SIGN_GREEN),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# 
# graph_eglise <- function(NUM){
#   ggplot()+
#     geom_sf(data=bdtopo %>%filter(NATURE=="Eglise") %>%  filter(row_number()==NUM ) %>% st_transform(4326),color="red",size=2  )+
#     geom_sf(data=bdtopo %>%
#               filter(NATURE=="Eglise") %>%
#               filter(row_number()==NUM ) %>%
#               st_transform(4326) %>%
#               st_segments() %>%
#               group_by(ID) %>%
#               mutate(max=max(st_length(result))) %>%
#               filter(st_length(result)==max)%>% rename(geometry=result),color="black",size=2)
#   ggsave(paste0("sorties/eglises","/eglise",NUM,".jpg"))
#   
# }
# bdtopo %>%filter(NATURE=="Eglise") %>% dim.data.frame()
# liste <- rep(1:841)
# walk(liste,graph_eglise)
# 
# mur_principal <- bdtopo %>% 
#   filter(NATURE=="Eglise" #| USAGE1 =="Religieux"
#   ) %>% 
#   st_transform(4326) %>% 
#   st_segments() %>%
#   group_by(ID) %>% 
#   mutate(max=max(st_length(result))) %>% 
#   filter(st_length(result)==max) %>% 
#   ungroup() %>% 
#   st_coordinates() %>% 
#   as_tibble() %>% 
#   group_by(L1) %>% 
#   mutate(id_segment=order(X) )%>% 
#   ungroup() %>% 
#   pivot_wider(id_cols = L1,names_from=id_segment,values_from=c(X,Y))
# 
# orientation <- mur_principal %>% 
#   mutate(azimuth=bearing(
#     .[,c("X_1","Y_1")],
#     .[,c("X_2","Y_2")]
#   ),
#   azimuth=if_else(azimuth<90,azimuth+180,azimuth)) %>% 
#   count(azi=plyr::round_any(azimuth,10)) %>% 
#   mutate(pct=n/sum(n)*100,
#          x=  cos(azi*pi/180),
#          y=  sin(azi*pi/180)
#   )
# 
# ggplot(data=orientation)+
#   geom_segment(aes(x=x,y=y,xend=0,yend=0,size=pct),color="sienna4")+
#   #geom_point(aes(x = x,y=y,size=pct),color="red",alpha=.6)+
#   xlim(c(-1,1))+
#   ylim(c(-1,1))+
#   theme_minimal()
