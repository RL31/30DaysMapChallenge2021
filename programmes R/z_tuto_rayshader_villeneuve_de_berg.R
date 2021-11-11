#https://www.tylermw.com/a-step-by-step-guide-to-making-3d-maps-with-satellite-imagery-in-r/

library(rayshader)
library(sp)
library(raster)
library(scales)

# 
# elevation1 = raster::raster("donnees/tuto_rayshader/N37W113.hgt")
# elevation2 = raster::raster("donnees/LC08_L1TP_038034_20191117_20191202_01_T1/N37W114.hgt")
# remplacer par mnt


# zion_elevation = raster::merge(elevation1,elevation2)
# 
# height_shade(raster_to_matrix(x)) %>%
#   plot_map()

#villeneuve <- raster("donnees/tuto_rayshader/vill_de_berg_claduegne_georeferenced.tif")
zion_rbg_corrected <- brick("donnees/tuto_rayshader/vill_de_berg_claduegne_georeferenced.tif")

raster::plotRGB(zion_rbg_corrected)

raster::crs(zion_rbg_corrected)

proj4string(altitudes_dpt) =CRS("+init=epsg:2154")
zion_rbg_corrected = raster::projectRaster(zion_rbg_corrected, crs = crs(altitudes_dpt), method = "bilinear")

raster::crs(altitudes_dpt)
raster::crs(zion_rbg_corrected)
# 
# zion_rbg_corrected = raster::projectRaster(zion_rbg_corrected, crs = crs(altitudes_dpt), method = "bilinear")

bottom_left=c(x=617722.5415,y=4935044.7875)
top_right=c(x=619208.3105,y=4936196.4995)
# bottom_left = c(y=44.835726,x=4.202614) #bbox google map
# top_right   = c(y=44.848506,y=4.236259)

extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right),
                                   proj4string=sp::CRS("+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs "))
extent_utm = sp::spTransform(extent_latlong, raster::crs(altitudes_dpt))


e = raster::extent(extent_utm)
e

zion_rgb_cropped = raster::crop(zion_rbg_corrected, e)
elevation_cropped = raster::crop(altitudes_dpt, e)

names(zion_rgb_cropped) = c("r","g","b")

zion_r_cropped = rayshader::raster_to_matrix(zion_rgb_cropped$r)
zion_g_cropped = rayshader::raster_to_matrix(zion_rgb_cropped$g)
zion_b_cropped = rayshader::raster_to_matrix(zion_rgb_cropped$b)

zionel_matrix = rayshader::raster_to_matrix(elevation_cropped)

zion_rgb_array = array(0,dim=c(nrow(zion_r_cropped),ncol(zion_r_cropped),3))

zion_rgb_array[,,1] = zion_r_cropped/255 #Red layer
zion_rgb_array[,,2] = zion_g_cropped/255 #Blue layer
zion_rgb_array[,,3] = zion_b_cropped/255 #Green layer

zion_rgb_array = aperm(zion_rgb_array, c(2,1,3))

plot_map(zion_rgb_array)

zion_rgb_contrast = scales::rescale(zion_rgb_array,to=c(0,1))

plot_map(zion_rgb_contrast)

plot_3d(zion_rgb_contrast, zionel_matrix, windowsize = c(1000,700), zscale = 10, shadowdepth = -50,
        zoom=.4, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
render_snapshot(title_text = "Zion National Park, Utah | Imagery: Landsat 8 | DEM: 30m SRTM",
                title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)


# terrils de sain étienne