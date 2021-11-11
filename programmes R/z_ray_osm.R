library(rayshader)
library(raster)
library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)


#pdl_petit = resize_matrix(matrice_pdl,0.25)


base_map <- matrice_pdl %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(matrice_pdl, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(matrice_pdl,zscale=8), 0) %>%
  add_shadow(texture_shade(matrice_pdl,detail=8/10,contrast=9,brightness = 11), 0.1) %>%
  plot_map()


# matrice_pdl %>% 
#   height_shade() %>% 
#   add_overlay(sphere_shade(matrice_pdl, texture = "desert", 
#                            zscale=4, colorintensity = 5), alphalayer=0.5) %>%
#   add_shadow(lamb_shade(matrice_pdl,zscale=8), 0) %>%
#   add_shadow(ambient_shade(matrice_pdl), 0) %>%
#   add_shadow(texture_shade(matrice_pdl,detail=8/10,contrast=9,brightness = 11), 0.1) %>%
#   plot_map()


osm_bbox <- st_bbox(pont_labeaume %>% st_transform(4326)) %>% as.numeric()

pdl_highway = opq(osm_bbox) %>% 
  add_osm_feature("highway") %>% 
  osmdata_sf() 

crs(ok) <- 2154

pdl_lines = st_transform(pdl_highway$osm_lines, crs=crs(ok))

# ggplot(pdl_lines,aes(color=osm_id)) + 
#   geom_sf() +
#   theme(legend.position = "none") +
#   labs(title = "Open Street Map `highway` attribute in Bryce Canyon National Park")
ext <- st_bbox(pont_labeaume) %>% as.numeric()


lat_range   = c(osm_bbox[2],osm_bbox[4])
long_range = c(osm_bbox[1],osm_bbox[3])

convert_coords = function(lat,long, from = CRS("+init=epsg:4326"), to) {
  data = data.frame(long=long, lat=lat)
  coordinates(data) <- ~ long+lat
  proj4string(data) = from
  #Convert to coordinate system specified by EPSG code
  xy = data.frame(sp::spTransform(data, to))
  colnames(xy) = c("x","y")
  return(unlist(xy))
}

crs(ok)
utm_bbox = convert_coords(lat = lat_range, long=long_range, to = crs(ok))

base_map %>% 
  add_overlay(generate_line_overlay(pdl_lines,extent = utm_bbox,
                                    heightmap = matrice_pdl)) %>% 
  plot_map()
