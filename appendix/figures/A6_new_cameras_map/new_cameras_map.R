# clear memory
rm(list=ls())
gc()

#required packages
library(ggplot2)
library(Cairo)
library(ggmap)
library(rgdal)
library(rgeos)
library(ggsn)
library(png)
library(grid)

# inputs 
  # roads
  roads.dsn <- "data/roads/speed_change_roads"
  roads.path <- "speed_change_roads"
  # cameras
  TD.p <- "data/tickets/tickets_over_segments.rds"
  
  # city area
  city.dsn <- "data/city_area"
  city.path <- "DEINFO_MUNICIPIO"


# output
OD.p <- "appendix/figures/A6_new_cameras_map/new_cameras_map.pdf"


# Main ----------------------------------------------------------------------------------------

# Cameras

  # read tickets
  TD <- readRDS(TD.p)

  # aggregate at camera level (finding minimum date of each camera, and keeping lat longs)
  TD$min.date <- as.Date(ave(as.numeric(TD$date), TD$place.id, FUN=min), origin = "1970-01-01")
  CD <- TD[!duplicated(TD$place.id), c("place.id", "min.date", "lat3", "long3","tr_group")]
  CD$tr_group <- ifelse(CD$tr_group %in% "tr", "tr", "co")
  
  # installation group
  CD$date.group <- ifelse(CD$min.date >= as.Date("2015-07-20"), "after", "before")
  CDaf <- CD[CD$date.group == "after",]
  
# Roads
  #read shapes
  RD.sp <- readOGR(roads.dsn, roads.path)
  RD <- fortify(RD.sp)
  RD$road <- "TR"
  
  # City Area
  # read kml
  CA.sp <- readOGR(city.dsn, city.path)
  CA.sp <- spTransform(CA.sp, "+proj=longlat +ellps=WGS84")
  CA <- fortify(CA.sp)

  
# map
  mybox <- c(left = -46.83, bottom = -23.8, right = -46.362, top = -23.37)
  mymap <- get_stamenmap(mybox, zoom = 8, maptype = "toner-lines")
  
  mapbox <- as.data.frame(matrix(nrow = 4, ncol = 2))
  mapbox$x <- c(-46.83, -46.83, -46.362, -46.362)
  mapbox$y <- c(-23.8, -23.37, -23.37, -23.8)
  
  #scale
  img <- readPNG("figures/3_map_and_raw/scale.png")
  g_pic <- rasterGrob(img, interpolate=TRUE)
  
# plot cameras installed after july,20-2015
  ggplot() +
   geom_path(aes(x=long, y = lat, group = group, colour = "road"), data = RD) +
   geom_path(aes(x=long, y = lat, group = group, colour = "sp"), data = CA, alpha = 0.5) +
   geom_point(aes(x=long3, y = lat3, colour = tr_group),
              data = CDaf,alpha = 0.3) +
   scale_colour_manual(values = c("road" = "gold", "tr" = "red", "co" = "black", "sp" = "forestgreen"),
                       labels = c("road" = "Roads With\nSpeed Limit  \nReduction",
                                  "tr" = "Cameras Installed  \non Roads With\nSpeed Limit\nReduction",
                                  "co" = "Cameras Installed  \non Other Roads",
                                  "sp" = "São Paulo\nCity Limits"),
                       name = "Legend:  ",
                       limits = c("road", "tr", "co", "sp"),
                       guide = guide_legend(override.aes = list(linetype = c("solid", "blank", "blank", "solid"),
                                                                shape = c(NA, 16, 16, NA)))) +
   theme_void() +
   theme(legend.title=element_text(size=8, face = "bold"),
         legend.text=element_text(size=8),
         legend.position="top") +
   coord_cartesian(xlim = c(-46.81,-46.364), ylim = c(-23.78, -23.39)) +
   annotation_custom(g_pic, xmin=-46.464, xmax=-46.364, ymin=-23.78, ymax=-23.68)
  
  ggsave(filename = OD.p, w = 5, h = 6)
  
  