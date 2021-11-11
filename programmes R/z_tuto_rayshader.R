#https://www.tylermw.com/a-step-by-step-guide-to-making-3d-maps-with-satellite-imagery-in-r/

library(rayshader)
library(sp)
library(raster)
library(scales)
library(tidyverse)
library(rgdal)

liste_asc <- list.files ("donnees/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D007/",
                         pattern = "*.asc", full.names = T) 

# f<-list.files(pattern=".asc", full.names = TRUE)
bd_alti_raster<-lapply(liste_asc, raster)
for(i in 2:length(bd_alti_raster)){
  altitudes_dpt<-merge(x=bd_alti_raster[[1]],y=bd_alti_raster[[i]],tolerance=5000,overlap=T)
  bd_alti_raster[[1]]<-altitudes_dpt
}

height_shade(raster_to_matrix(altitudes_dpt)) %>%
  plot_map()

#############

  plot_map(raster_to_matrix(r))
essai <- raster("donnees/tuto_rayshader/BDORTHO/07-1953-0845-6425-LA93-0M50-E080.jp2")
install.packages("gdalUtils",dependencies = TRUE)
library(gdalUtils)
library(rgdal)


r <- readGDAL("donnees/tuto_rayshader/BDORTHO/07-1953-0845-6425-LA93-0M50-E080.jp2")
gdal
gdal_translate("donnees/tuto_rayshader/BDORTHO/07-1953-0845-6425-LA93-0M50-E080.jp2", "new_band_name.tif")
band1 <- raster::brick("new_band_name.tif")
raster::plotRGB(band1)

library(raster)
r <- raster(matrix(rnorm(100), 10))
plot(band1, col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL))

#############


vue_aerienne_rouge <-  raster::raster("donnees/tuto_rayshader/LC08_L1TP_197029_20210711_20210720_01_T1_B4.TIF")
vue_aerienne_vert <-  raster::raster("donnees/tuto_rayshader/LC08_L1TP_197029_20210711_20210720_01_T1_B3.TIF")
vue_aerienne_bleu <-  raster::raster("donnees/tuto_rayshader/LC08_L1TP_197029_20210711_20210720_01_T1_B2.TIF")

vue_aerienne_rvb <-  raster::stack(vue_aerienne_rouge, vue_aerienne_vert, vue_aerienne_bleu)
raster::plotRGB(vue_aerienne_rvb, scale=255^2)

vue_aerienne_rvb_corrige <-  sqrt(raster::stack(vue_aerienne_rouge, vue_aerienne_vert, vue_aerienne_bleu))
raster::plotRGB(vue_aerienne_rvb_corrige)

raster::crs(vue_aerienne_rouge)
raster::crs(altitudes_dpt)
proj4string(altitudes_dpt) <- CRS("+init=epsg:2154")

altitudes_dpt_utm <-  raster::projectRaster(altitudes_dpt, crs = crs(vue_aerienne_rouge), method = "bilinear")
crs(altitudes_dpt_utm)

bas_gauche <- c(x=582372.7400,y=4961733.0894)
haut_droite=c(x=598509.4912,y=4968452.5939)

etendue_utm <-  sp::SpatialPoints(rbind(bas_gauche, haut_droite),
                                 proj4string=sp::CRS("+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs "))
#extent_utm = sp::spTransform(extent_latlong, raster::crs(x_utm))

etendue <-  raster::extent(etendue_utm)
etendue

vue_aerienne_decoupee <-  raster::crop(vue_aerienne_rvb_corrige, etendue)
altitude_decoupee <-  raster::crop(altitudes_dpt_utm, etendue)

names(vue_aerienne_decoupee) <-  c("r","g","b")

vue_aerienne_rouge_decoupe <-  rayshader::raster_to_matrix(vue_aerienne_decoupee$r)
vue_aerienne_vert_decoupe <-  rayshader::raster_to_matrix(vue_aerienne_decoupee$g)
vue_aerienne_bleu_decoupe <-  rayshader::raster_to_matrix(vue_aerienne_decoupee$b)

altitude_decoupee_matrice <-  rayshader::raster_to_matrix(altitude_decoupee)

vue_aerienne_rvb_array <-  array(0,dim=c(nrow(vue_aerienne_rouge_decoupe),ncol(vue_aerienne_rouge_decoupe),3))

vue_aerienne_rvb_array[,,1] <-  vue_aerienne_rouge_decoupe/255 #Red layer
vue_aerienne_rvb_array[,,2] <-  vue_aerienne_vert_decoupe/255 #Blue layer
vue_aerienne_rvb_array[,,3] <-  vue_aerienne_bleu_decoupe/255 #Green layer

vue_aerienne_rvb_array <-  aperm(vue_aerienne_rvb_array, c(2,1,3))

plot_map(vue_aerienne_rvb_array)

vue_aerienne_rvb_contraste <-  scales::rescale(vue_aerienne_rvb_array,to=c(0,1))

plot_map(vue_aerienne_rvb_contraste)

plot_3d(vue_aerienne_rvb_contraste, altitude_decoupee_matrice,
        windowsize = c(1000,700), zscale = 25, shadowdepth = -50,
        zoom=.4, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")

render_snapshot(title_text = "Zion National Park, Utah | Imagery: Landsat 8 | DEM: 30m SRTM",
                title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)


# terrils de sain étienne