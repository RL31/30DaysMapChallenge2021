#https://www.tylermw.com/a-step-by-step-guide-to-making-3d-maps-with-satellite-imagery-in-r/

library(rayshader)
library(sp)
library(raster)
library(scales)

# image georeferencee
rhone <- brick("donnees/tuto_rayshader/Rhone_a_glun_mauves_1784_georef.tif")
plotRGB(rhone)

liste_asc <- list.files ("donnees/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D007/",
                         pattern = "*.asc", full.names = T) 

# f<-list.files(pattern=".asc", full.names = TRUE)
bd_alti_raster<-lapply(liste_asc, raster)
for(i in 2:length(bd_alti_raster)){
  altitudes_dpt<-merge(x=bd_alti_raster[[1]],y=bd_alti_raster[[i]],tolerance=5000,overlap=T)
  bd_alti_raster[[1]]<-altitudes_dpt
}

proj4string(altitudes_dpt) <- CRS("+init=epsg:2154")
altitudes_dpt_utm <-  raster::projectRaster(altitudes_dpt, crs = crs(rhone), method = "bilinear")
# crs(altitudes_dpt_utm)
pour_etendue <- brick("donnees/tuto_rayshader/pour_etendue_georeferenced.tif")

etendue <-  raster::extent(pour_etendue)

altitude_decoupee <-  raster::crop(altitudes_dpt_utm, etendue)

plot(altitudes_dpt_utm)
plot(altitude_decoupee)

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

altitude_decoupee_matrice <-  rayshader::raster_to_matrix(altitude_decoupee2)

#rhone_matrice <- scales::rescale(raster_to_matrix(rhone2),to=c(0,1))

rhone_r = rayshader::raster_to_matrix(rhone2$Rhone_a_glun_mauves_1784_georef.1)
rhone_g = rayshader::raster_to_matrix(rhone2$Rhone_a_glun_mauves_1784_georef.2)
rhone_b = rayshader::raster_to_matrix(rhone2$Rhone_a_glun_mauves_1784_georef.3)

rhone_array = array(0,dim=c(nrow(rhone_r),ncol(rhone_r),3))

rhone_array[,,1] = rhone_r/255 #Red layer
rhone_array[,,2] = rhone_g/255 #Blue layer
rhone_array[,,3] = rhone_b/255 #Green layer

rhone_array = aperm(rhone_array, c(2,1,3))

rhone_contrast = scales::rescale(rhone_array,to=c(0,1))

plot_3d(rhone_contrast, altitude_decoupee_matrice,
        windowsize = c(1000,750), zscale = 25, shadowdepth = -50,
        zoom=.3, phi=40,theta=-90,fov=110, background = "#F2E1D0", shadowcolor = "#523E2B")

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
