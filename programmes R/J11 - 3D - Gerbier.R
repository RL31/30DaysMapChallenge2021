remotes::install_github("tylermorganwall/rayshader")


library(rgdal)
library(rayshader)
library(sp)
library(raster)
library(scales)
library(tidyverse)

liste_asc <- list.files ("donnees/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D007/",
                         pattern = "*.asc", full.names = T) 

# f<-list.files(pattern=".asc", full.names = TRUE)
bd_alti_raster<-lapply(liste_asc, raster)
for(i in 2:length(bd_alti_raster)){
  altitudes_dpt<-merge(x=bd_alti_raster[[1]],y=bd_alti_raster[[i]],tolerance=5000,overlap=T)
  bd_alti_raster[[1]]<-altitudes_dpt
}


# liste <- list.files(path = "donnees/tuto_rayshader/BDORTHO/",
#            pattern = ".jp2")
# 
# pont de labeaum = 07-1953-0800-6400-LA93-0M50-E080.jp2

#Gerbier
#s2a <- raster(readGDAL("donnees/tuto_rayshader/BDORTHO/07-1953-0795-6420-LA93-0M50-E080.jp2"))
s2a <- raster(readGDAL("donnees/tuto_rayshader/BDORTHO/07-1953-0800-6400-LA93-0M50-E080.jp2"))

#vallon
s2a <- raster(readGDAL("donnees/tuto_rayshader/BDORTHO/07-1953-0810-6370-LA93-0M50-E080.jp2"))
# raster::crs(s2a)
# raster::crs(altitudes_dpt)
proj4string(altitudes_dpt) <- CRS("+init=epsg:2154")

altitudes_dpt_utm <-  raster::projectRaster(altitudes_dpt, crs = crs(s2a), method = "bilinear")
# crs(altitudes_dpt_utm)

#Pont de labeaume
bas_gauche <- c(x=4.283638,y=44.662457)
haut_droite=c(x=4.297607,y=44.671995)

# gerbier
bas_gauche <- c(x=4.212828,y=44.839165)
haut_droite=c(x=4.226604,y=44.847198)

# vallon
bas_gauche <- c(x=4.410539,y=44.378349)
haut_droite=c(x=4.422770,y=44.386415)

etendue_utm <-  sp::SpatialPoints(rbind(bas_gauche, haut_droite),
                                  proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
extent_utm = sp::spTransform(etendue_utm, raster::crs(s2a))

etendue <-  raster::extent(extent_utm)
# etendue

altitude_decoupee <-  raster::crop(altitudes_dpt_utm, etendue)
s2a_decoupe <-  raster::crop(s2a, etendue)


altitude_decoupee2 <- raster::projectRaster(altitude_decoupee, crs = crs("+proj=lcc
+towgs84=0.0000,0.0000,0.0000 +a=6378137.0000 +rf=298.2572221010000
+lat_0=46.500000000
+lon_0=3.000000000
+lat_1=44.000000000
+lat_2=49.000000000
+x_0=700000.000
+y_0=6600000.000
+units=m
+no_defs"), method = "bilinear")

s2a_decoupe2 <- raster::projectRaster(s2a_decoupe, crs = crs("+proj=lcc
+towgs84=0.0000,0.0000,0.0000 +a=6378137.0000 +rf=298.2572221010000
+lat_0=46.500000000
+lon_0=3.000000000
+lat_1=44.000000000
+lat_2=49.000000000
+x_0=700000.000
+y_0=6600000.000
+units=m
+no_defs"), method = "bilinear")

altitude_decoupee_matrice <-  rayshader::raster_to_matrix(altitude_decoupee2)

s2a_bis <- scales::rescale(raster_to_matrix(s2a_decoupe2),to=c(0,1))

# alti2 <- scales::rescale(altitude_decoupee_matrice,to=c(0,1))
#le raster to mat
# plot_map(scales::rescale(rayshader::raster_to_matrix(s2a_decoupe),to=c(0,1)))

# 
# plot_map(s2a_bis)
# plot_map(scales::rescale(altitude_decoupee_matrice,to=c(0,1)))
# plot(s2a_decoupe2)

s2a_bis <- s2a_bis[,ncol(s2a_bis):1]  
 
plot_3d(s2a_bis, altitude_decoupee_matrice,
        windowsize = c(1000,1500), zscale = 25, shadowdepth = 0,
        zoom=.6, phi=30,theta=-90,fov=100, background = "#F2E1D0", shadowcolor = "#523E2B")

render_camera(zoom=.5)


render_snapshot(title_text = "Le Mont Gerbier de Jonc en 1953",
                title_bar_color = "coral3", title_color = "white",
                title_bar_alpha = 1,
                vignette = TRUE,
                title_font = "Calibri"

                )

render_snapshot(title_text = "La vallée de l'Ardèche à Pont-de-Labeaume en 1953\nIGN, BD ORTHO",
                title_bar_color = "#F2E1D0", title_color = "gray20", title_bar_alpha = 1)

