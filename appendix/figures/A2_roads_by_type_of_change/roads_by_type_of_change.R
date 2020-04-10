# clear memory
rm(list=ls())
gc()

#required packages
library(ggplot2)
library(Cairo)
library(ggmap)
library(rgdal)
library(ggsn)
library(png)
library(grid)

# inputs 
  # roads
  roads.dsn <- "data/roads/speed_change_roads"
  roads.path <- "speed_change_roads"
  
  # city area
  city.dsn <- "data/city_area"
  city.path <- "DEINFO_MUNICIPIO"
  

# output
OD.p <- "appendix/figures/A2_roads_by_type_of_change/roads_by_type_of_change.pdf"


# Main ----------------------------------------------------------------------------------------


# Roads
  #read shapes
  RD <- readOGR(roads.dsn, roads.path)
  RD@data$type_of_change <- paste("NS", RD@data$NewSpeed)
  RD@data$type_of_change <- ifelse(RD@data$type_of_change %in% c("NS 40", "NS 50", "NS 60"),
                                   "NS 50", "NS 70")

  RD@data$id <- row.names(RD@data)
  RD.A <- RD@data
  
  RDD <- fortify(RD)
  
  RDD2 <- merge(RDD, RD.A, by = "id", all.x = T)
  
  
  # City Area
  # read kml
  CA.sp <- readOGR(city.dsn, city.path)
  CA.sp <- spTransform(CA.sp, "+proj=longlat +ellps=WGS84")
  CA <- fortify(CA.sp)
  
# map
  mybox <- c(left = -46.83, bottom = -23.8, right = -46.362, top = -23.37)
  mymap <- get_stamenmap(mybox, zoom = 10, maptype = "toner-lines")
  
  mapbox <- as.data.frame(matrix(nrow = 4, ncol = 2))
  mapbox$x <- c(-46.83, -46.83, -46.362, -46.362)
  mapbox$y <- c(-23.8, -23.37, -23.37, -23.8)
  
#scale
  img <- readPNG("figures/_aux/scale.png")
  g_pic <- rasterGrob(img, interpolate=TRUE)

# plot map
ggplot() +
 geom_path(aes(x=long, y = lat, group = group, colour = type_of_change), data = RDD2, alpha = 0.8) +
 geom_path(aes(x=long, y = lat, group = group, colour = "sp"), data = CA, alpha = 0.5) +
 scale_color_manual(labels = c("NS 70" = "70km/h\n(Marginais Highways)        ",
                               "NS 50" = "50km/h\n(Arterial Roads)",
                               "sp" = "\nSão Paulo\nCity Limits"),
                    values = c("NS 70" = "tomato2",
                               "NS 50" = "pink",
                               "sp" = "black"),
                    guide = guide_legend(override.aes = list(linetype = c(rep("solid", 3)),
                                                             shape = c(rep(NA, 3)))),
                    name = "New Speed Limit:") +
 theme_void() +
 theme(legend.title=element_text(size=9, face = "bold"),
       legend.text=element_text(size=8),
       legend.position="top") +
 guides(col=guide_legend(ncol=3)) +
 coord_cartesian(xlim = c(-46.81,-46.364), ylim = c(-23.78, -23.39)) +
 annotation_custom(g_pic, xmin=-46.464, xmax=-46.364, ymin=-23.78, ymax=-23.68)



ggsave(filename = OD.p, w = 5, h=6)

