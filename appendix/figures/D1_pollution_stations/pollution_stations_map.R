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
  
  
  # city area
  city.dsn <- "data/city_area"
  city.path <- "DEINFO_MUNICIPIO"
  
  # stations
  SDp <- "data/pollution/stations.csv"


# output
OD.p <- "appendix/figures/D1_pollution_stations/pollution_stations.pdf"


# Main ----------------------------------------------------------------------------------------

# Cameras

  # stations
  SD <- read.csv(SDp)
  SD <- SD[!(SD$tr_st == "co2"),]
  
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

  SD$tr_st[SD$tr_st == "tr2"] <- "co1"
# map
  mybox <- c(left = -46.95, bottom = -23.8, right = -46.1, top = -23.35)
  mymap <- get_stamenmap(mybox, zoom = 10, maptype = "toner-lines")
  
  mapbox <- as.data.frame(matrix(nrow = 4, ncol = 2))
  mapbox$x <- c(-46.95, -46.95, -46.1, -46.1)
  mapbox$y <- c(-23.8, -23.35, -23.35, -23.8)
  
# plot cameras installed after july,20-2015
  ggplot() +
   geom_polygon(aes(x = x, y = y),
                data = mapbox, alpha = 1, fill = "white") +
   #scalebar(x.min = min(mapbox$x)+0.075, x.max = max(mapbox$x)-0.075,
   #         y.min = min(mapbox$y)+0.03, y.max = max(mapbox$y)-0.03,
   #         dist = 5, dd2km = TRUE, model = "WGS84", st.size = 2) + 
   geom_path(aes(x=long, y = lat, group = group, colour = type_of_change), data = RDD2, alpha = 0.8) +
   geom_path(aes(x=long, y = lat, group = group, colour = "sp"), data = CA, alpha = 0.5) +
   geom_point(aes(x=long, y = lat, colour = tr_st),
              data = SD, size = 3) +
   scale_colour_manual(values = c("NS 70" = "darkgoldenrod",
                                  "NS 50" = "goldenrod1",
                                  "tr1" = "red",
                                  "co1" = "black",
                                  "sp" = "forestgreen"),
                       labels = c("NS 70" =  "Marginais\nHighways",
                                  "NS 50" =   "Arterial Roads",
                                  "tr1" = "Treated\nStation",
                                  "co1" = "Control\nStation",
                                  "sp" = "São Paulo\nCity Limits"),
                       name = "Legend:  ",
                       limits = c("tr1", "co1", "NS 70","NS 50", "sp"),
                       guide = guide_legend(override.aes = list(linetype = c("blank", "blank", "solid", "solid", "solid"),
                                                                shape = c(16, 16, NA, NA, NA)),
                                            ncol = 5)) + 
                     
   theme_void() +
   theme(legend.title=element_text(size=8, face = "bold"),
         legend.text=element_text(size=8),
         legend.position="top") +
   coord_cartesian(xlim = c(-46.95,-46.1), ylim = c(-23.8, -23.35))
  
  ggsave(filename = OD.p, w = 8, h = 6)