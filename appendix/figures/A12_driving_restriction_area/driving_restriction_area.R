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


# inputs 
  # roads
  roads.dsn <- "data/roads/speed_change_roads"
  roads.path <- "speed_change_roads"
  
  # rodizio area
  rodiz.dsn <- "data/roads"
  rodiz.path <- "rodizio"
  
  # city area
  city.dsn <- "data/city_area"
  city.path <- "DEINFO_MUNICIPIO"
  


# output
OD.p <- "appendix/figures/A12_driving_restriction_area/driving_restriction_area.pdf"


# Main ----------------------------------------------------------------------------------------
# Cameras
  
# Roads
  #read shapes
  RD.sp <- readOGR(roads.dsn, roads.path)
  RD <- fortify(RD.sp)
  RD$road <- "TR"

# Rodizio
  # read kml
  RZ.sp <- readOGR(rodiz.dsn, rodiz.path)
  RZ <- fortify(RZ.sp)
  
  
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
  
# plot cameras installed after july,20-2015
  ggplot() +
   geom_polygon(aes(x = x, y = y),
                data = mapbox, alpha = 1, fill = "white") +
   scalebar(x.min = min(mapbox$x)+0.05, x.max = max(mapbox$x)-0.05,
            y.min = min(mapbox$y)+0.02, y.max = max(mapbox$y)-0.02,
            dist = 5, dd2km = TRUE, model = "WGS84", st.size = 2) + 
   geom_path(aes(x=long, y = lat, group = group, colour = "road"), data = RD) +
   geom_path(aes(x=long, y = lat, group = group, colour = "sp"), data = CA, alpha = 0.5) +
   geom_polygon(aes(x=long, y = lat, group = group, fill = "rodiz"),
                data = RZ, alpha = 0.1) +
   scale_colour_manual(values = c("road" = "gold", "sp" = "forestgreen"),
                       labels = c("road" = "Roads With\nSpeed Limit    \nReduction",
                                  "sp" = "São Paulo\nCity\nLimits      "),
                       name = "Legend:        ") +
   scale_fill_manual(values = c("rodiz" = "red"),
                     labels = c("rodiz" = "Driving\nRestriction\nArea    "),
                     name = "",
                     limits = c("rodiz")) +
   theme_void() +
   theme(legend.title=element_text(size=9, face = "bold"),
         legend.text=element_text(size=8),
         legend.position="top") +
   coord_cartesian(xlim = c(-46.8,-46.4), ylim = c(-23.7, -23.45))
  ggsave(filename = OD.p, w = 5, h = 4)
  
  
  
# identify ratio of treated roads on Rodizio Area
RZ.sp <- spTransform(RZ.sp, paste0("+proj=merc +lon_0=0 ",
                                  "+lat_ts=6.644567463596037 ",
                                  "+x_0=0 +y_0=0 +ellps=GRS80 ",
                                  "+units=m +no_defs"))
buff <- gBuffer(RZ.sp, width = 100)
RD.sp <- spTransform(RD.sp, paste0("+proj=merc +lon_0=0 ",
                                  "+lat_ts=6.644567463596037 ",
                                  "+x_0=0 +y_0=0 +ellps=GRS80 ",
                                  "+units=m +no_defs"))
chopped <- gIntersection(buff, RD.sp)
a <- SpatialLinesLengths(chopped, longlat = F)/1000
b <- sum(SpatialLinesLengths(RD.sp, longlat = F))/1000
a/b

