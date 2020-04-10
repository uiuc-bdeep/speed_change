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
library(ggspatial)

library(png)
library(grid)

# inputs 
  # roads
  roads.dsn <- "data/roads/speed_change_roads"
  roads.path <- "speed_change_roads"
  
  # buffers
  b.dsn <- "data/roads/speed_change_roads/buffers"
  b.layer <- "buffer_200"
  # rings
  r.dsn <- "data/roads/speed_change_roads/rings"
  r1.layer <- "ring_1000"
  r3.layer <- "ring_3000"
  r5.layer <- "ring_5000"
  r7.layer <- "ring_7000"
  
  
  # city area
  city.dsn <- "data/city_area"
  city.path <- "DEINFO_MUNICIPIO"
  


# output
  OD.p <- "appendix/figures/A9_travel_time_spillover_area/travel_time_spillover_area.pdf"
 

# Main ----------------------------------------------------------------------------------------

# Roads
  #read buffers
  BS <- readOGR(b.dsn, b.layer)
  BS <- spTransform(BS, "+proj=longlat +ellps=WGS84")
  B <- fortify(BS)
  B$area <- "MA"
  
  R1S <- readOGR(r.dsn, r1.layer)
  R3S <- readOGR(r.dsn, r3.layer)
  R5S <- readOGR(r.dsn, r5.layer)
  R7S <- readOGR(r.dsn, r7.layer)
  
  R1S <- spTransform(R1S, "+proj=longlat +ellps=WGS84")
  R3S <- spTransform(R3S, "+proj=longlat +ellps=WGS84")
  R5S <- spTransform(R5S, "+proj=longlat +ellps=WGS84")
  R7S <- spTransform(R7S, "+proj=longlat +ellps=WGS84")
  
  
  R1 <- fortify(R1S)
  R3 <- fortify(R3S)
  R5 <- fortify(R5S)
  R7 <- fortify(R7S)
  
  R1$area <- "R1"
  R3$area <- "R3"
  R5$area <- "R5"
  R7$area <- "R7"

  #roads
  RD.sp <- readOGR(roads.dsn, roads.path)
  MA.sp <- RD.sp[as.character(RD.sp$ChangeDate) == "2015/07/20",]
  MA <- fortify(MA.sp)
  MA$road <- "MA"
  
  RD <- fortify(RD.sp)
  RD$road <- "OT"

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
   geom_polygon(aes(x=long, y = lat, group = group, fill = area), data = R5, alpha = 0.75) +
   geom_polygon(aes(x=long, y = lat, group = group), data = R3, fill = "white") +
   geom_polygon(aes(x=long, y = lat, group = group, fill = area), data = R3, alpha = 0.75) +
   geom_polygon(aes(x=long, y = lat, group = group), data = R1, fill = "white") +
   geom_polygon(aes(x=long, y = lat, group = group, fill = area), data = R1, alpha = 0.75) +
   geom_polygon(aes(x=long, y = lat, group = group), data = B, fill = "white") +
   geom_polygon(aes(x=long, y = lat, group = group, fill = area), data = B, alpha = 0.75) +
   geom_path(aes(x=long, y = lat, group = group, colour = "ot"), data = RD, alpha = 0.25) +
   geom_path(aes(x=long, y = lat, group = group, colour = "ma"), data = MA) +
   geom_path(aes(x=long, y = lat, group = group, colour = "sp"), data = CA, alpha = 0.5) +
   scale_colour_manual(values = c("ma" = "black", "ot" = "darkgray", "sp" = "black"),
                       labels = c("ma" = "Marginais    ", "ot" = "Arterial\nRoads    ",
                                  "sp" = "São Paulo\nCity Limits"),
                       name = " Legend:    ") +
   scale_fill_manual(values = c("MA" = "black", "R1" = "firebrick", 
                                "R3" = "lightsalmon", "R5" = "gold"),
                     labels = c("MA" = "0.2 km", "R1" = "1 km", 
                                "R3" = "3 km", "R5" = "5 km"),
                     name = "Distance to\nMarginais:        ",
                     limits = c("MA", "R1", "R3", "R5")) +
   theme_void() +
   theme(legend.title=element_text(size=9, face = "bold"),
         legend.text=element_text(size=8),
         legend.position="top",
         legend.box = "vertical",
         legend.direction = "horizontal") +
   coord_cartesian(xlim = c(-46.8,-46.4), ylim = c(-23.7, -23.45)) +
   annotation_custom(g_pic, xmin=-46.6, xmax=-46.4, ymin=-23.7, ymax=-23.66)
  ggsave(filename = OD.p, w = 5, h = 4)
  
  