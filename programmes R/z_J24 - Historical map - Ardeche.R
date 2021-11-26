#https://www.tylermw.com/a-step-by-step-guide-to-making-3d-maps-with-satellite-imagery-in-r/

library(rayshader)
library(sf)
library(tidyverse)
library(sp)
library(raster)
library(scales)

# image georeferencee
rhone <- brick("donnees/Malte_Brun/scan_ardeche_ok_georef_lineaire4326.tif")

proj4string(rhone) <- CRS("+init=epsg:2154")

liste_asc <- list.files ("donnees/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D007/",
                         pattern = "*.asc", full.names = T) 

# f<-list.files(pattern=".asc", full.names = TRUE)
bd_alti_raster<-lapply(liste_asc, raster)
for(i in 2:length(bd_alti_raster)){
  altitudes_dpt<-merge(x=bd_alti_raster[[1]],y=bd_alti_raster[[i]],tolerance=5000,overlap=T)
  bd_alti_raster[[1]]<-altitudes_dpt
}

# crs(rhone)
proj4string(altitudes_dpt) <- CRS("+init=epsg:2154")
# rhone_2154 <-  raster::projectRaster(rhone, crs = crs(altitudes_dpt), method = "bilinear")

# altitudes_dpt_utm <-  raster::projectRaster(altitudes_dpt, crs = crs(rhone), method = "bilinear")
# crs(altitudes_dpt_utm)
departement <- st_read("donnees/DEPARTEMENT.shp") %>% 
  filter(INSEE_DEP=="07")
  st_transform(4326)

etendue <-  raster::extent(departement)

altitude_decoupee <-  raster::crop(altitudes_dpt, etendue)

alt2 <- mask(altitude_decoupee,departement)

# plot(altitudes_dpt_utm)
# plot(altitude_decoupee)
# 
# plot(alt2)
# plot(rhone$scan_ardeche_ok_georef_lineaire4326.1)
# plot(st_geometry(departement), add=TRUE, lwd=2,color=NA)




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

rhone2 <- raster::projectRaster(rhone, crs = crs("+proj=lcc
+towgs84=0.0000,0.0000,0.0000 +a=6378137.0000 +rf=298.2572221010000
+lat_0=46.500000000
+lon_0=3.000000000
+lat_1=44.000000000
+lat_2=49.000000000
+x_0=700000.000
+y_0=6600000.000
+units=m
+no_defs"), method = "bilinear")

altitude_decoupee_matrice <-  rayshader::raster_to_matrix(alt2)

#rhone_matrice <- scales::rescale(raster_to_matrix(rhone2),to=c(0,1))
rhone_r = rayshader::raster_to_matrix(rhone$scan_ardeche_ok_georef_lineaire4326.1)
rhone_g = rayshader::raster_to_matrix(rhone$scan_ardeche_ok_georef_lineaire4326.2)
rhone_b = rayshader::raster_to_matrix(rhone$scan_ardeche_ok_georef_lineaire4326.3)

rhone_array = array(0,dim=c(nrow(rhone_r),ncol(rhone_r),3))

rhone_array[,,1] = rhone_r/255 #Red layer
rhone_array[,,2] = rhone_g/255 #Blue layer
rhone_array[,,3] = rhone_b/255 #Green layer

rhone_array = aperm(rhone_array, c(2,1,3))

rhone_contrast = scales::rescale(rhone_array,to=c(0,1))

raster::plot(altitude_decoupee_matrice)


s2a_bis <- scales::rescale(raster_to_matrix(rhone),to=c(0,1))

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
        zoom=.1, phi=0,theta=0,fov=0, background = "#F2E1D0", shadowcolor = "#523E2B")



s2a_bis <- scales::rescale(raster_to_matrix(s2a_decoupe2),to=c(0,1))

# alti2 <- scales::rescale(altitude_decoupee_matrice,to=c(0,1))
#le raster to mat
# plot_map(scales::rescale(rayshader::raster_to_matrix(s2a_decoupe),to=c(0,1)))

# 
# plot_map(s2a_bis)
# plot_map(scales::rescale(altitude_decoupee_matrice,to=c(0,1)))
# plot(s2a_decoupe2)

s2a_bis <- s2a_bis[,ncol(s2a_bis):1]  


plot_map(rhone_contrast)
height_shade(altitude_decoupee_matrice) %>%
  plot_map()



plot_3d(rhone_contrast, altitude_decoupee_matrice,
        windowsize = c(300,300), zscale = 25, shadowdepth = -50,
        zoom=.1, phi=40,theta=-90,fov=110, background = "#F2E1D0", shadowcolor = "#523E2B")

render_snapshot( title_text = "Le Rhône entre Mauves et Glun (1784)",
                 title_bar_color = "cadetblue3", title_color = "white", title_bar_alpha = 1,
                 vignette = TRUE,
                 title_font = "Calibri")



angles= seq(0,360,length.out = 1441)[-1]
for(i in 1:1440) {
  render_camera(theta=-45+angles[i])
  render_snapshot(filename = sprintf("le_rhone%i.png", i), 
                  title_text = "Le Rhône entre Mauves et Glun (1784)",
                  title_bar_color = "cadetblue3", title_color = "white", title_bar_alpha = 1,
                  vignette = TRUE,
                  title_font = "Calibri")
}
rgl::rgl.close()

av::av_encode_video(sprintf("zionpark%d.png",seq(1,1440,by=1)), framerate = 30,
                    output = "zionpark.mp4")

rgl::rgl.close()
system("cmd.exe", input = "ffmpeg -framerate 60 -i zionpark%d.png -pix_fmt yuv420p zionpark.mp4")


av::av_encode_video(sprintf("zionpark%d.png",seq(1,10,by=1)), framerate = 30,output = "zionpark.mp4")
