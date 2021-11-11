# install.packages("rayshader",dependencies = T)
# 
# install.packages("Rcpp",dependencies = T)
# 
# 
# library(rayshader)
# 
# #Here, I load a map with the raster package.
# loadzip = tempfile() 
# download.file("https://tylermw.com/data/dem_01.tif.zip", loadzip)
# localtif = raster::raster(unzip(loadzip, "dem_01.tif"))
# unlink(loadzip)
# 
# #And convert it to a matrix:
# elmat = raster_to_matrix(elevation.sub)
# 
# #We use another one of rayshader's built-in textures:
# elmat %>%
#   sphere_shade(texture = "desert") %>%
#   plot_map()
# 
# 
# elmat %>%
#   sphere_shade(texture = "desert") %>%
#   add_water(detect_water(elmat), color = "desert") %>%
#   add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
#   add_shadow(ambient_shade(elmat), 0) %>%
#   plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
# Sys.sleep(0.2)
# render_snapshot()
# 
# 
# 
# library(raster)
# essai <-  raster("BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D007/BDALTIV2_25M_FXX_0750_6425_MNT_LAMB93_IGN69.asc")
# plot(essai)
# 
# 

library(raster)
library(rayshader)
library(sf)
library(tidyverse)
library(rgdal)

list_file <- list.files ("donnees/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D007/",
                         pattern = "*.asc", full.names = T) 

f<-list.files(pattern=".asc", full.names = TRUE)
r<-lapply(list_file, raster)
for(i in 2:length(r)){  
  x<-merge(x=r[[1]],y=r[[i]],tolerance=5000,overlap=T)  
  r[[1]]<-x  
} 



pont_labeaume <- st_read("donnees/COMMUNE.shp") %>% 
  filter(INSEE_COM %in% c("07178")) %>% 
  st_centroid() %>% 
  st_buffer(4000)#,"07156","07065") )

elevation.sub <- crop(x, extent(pont_labeaume))
ok <- mask(elevation.sub, pont_labeaume)

plot(ok)

matrice_pdl = raster_to_matrix(essai)#ok)
elmat %>%
  sphere_shade(texture = "imhof2") %>%
  plot_map()

#detect_water and add_water adds a water layer to the map:
elmat %>%
  sphere_shade(texture = "imhof2") %>%
  add_water(detect_water(elmat,), color = "red") %>%
  plot_map()


par(mfrow = c(1, 2))
montshadow = ray_shade(elmat, zscale = 50, lambert = FALSE)
montamb = ambient_shade(elmat, zscale = 50)
elmat %>%
  sphere_shade(zscale = 1, texture = "imhof2") %>%
  # add_shadow(montshadow, 0.5) %>%
  # add_shadow(montamb, 0) %>%
  plot_3d(elmat, zscale = 20, fov = 0, theta = -45, phi = 45, windowsize = c(1000, 800), zoom = .5,
          water = TRUE, waterdepth = 236.95, wateralpha = 0.5, watercolor = "lightblue",
          waterlinecolor = "white", waterlinealpha = 0.5, baseshape = "circle")

render_snapshot(clear = TRUE)

elmat %>%
  sphere_shade(zscale = 10, texture = "imhof1") %>%
  add_shadow(montshadow, 0.5) %>%
  add_shadow(montamb, 0) %>%
  add_water(detect_water(elmat,max_height = 400,min_area = 400), color = "lightblue") %>%
    plot_3d(elmat, zscale = 50, fov = 0, theta = -45, phi = 45, windowsize = c(1000, 800), zoom = 0.6,
           baseshape = "hex")
# 
# water = TRUE, waterdepth = 350, wateralpha = 0.5, watercolor = "lightblue",
# waterlinecolor = "white", waterlinealpha = 0.5,

render_snapshot(clear = TRUE)



elmat %>%
  sphere_shade(texture = "imhof2") %>%
  add_water(detect_water(elmat), color = "blue") %>%
    add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_3d(elmat,zscale = 10,
          fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
Sys.sleep(0.2)
render_snapshot()
