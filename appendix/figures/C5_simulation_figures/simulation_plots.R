# clear memory
rm(list=ls())

# required packages
library(rgeos)
library(rgdal)
library(ggplot2)
library(raster)
library(SDraw)
library(raster)
library(cowplot)
library(ggmap)

# own functions

# inputs ----------------------------------------------------------------------
# read routes
MB1 <- readRDS("data/osrm_simulations/segments/VMT1.rds")
MB2 <- readRDS("data/osrm_simulations/segments/VMT2.rds")
MB3 <- readRDS("data/osrm_simulations/segments/VMT3.rds")
PO <- readRDS("data/osrm_simulations/segments/points.rds")

# trip example
mt1 <- readRDS("data/osrm_simulations/routes/1.1_routes_1.rds")
mt2 <- readRDS("data/osrm_simulations/routes/1.2_routes_2.rds")
mt3 <- readRDS("data/osrm_simulations/routes/1.3_routes_3.rds")


PO <- merge(PO, MB1, by = "point_id")
PO <- merge(PO, MB2, by = "point_id")
PO <- merge(PO, MB3, by = "point_id")

PO$VMT1[PO$VMT1==0] <- 400
PO$VMT2[PO$VMT2==0] <- 400
PO$VMT3[PO$VMT3==0] <- 400

PO$VMT_12 <- (PO$VMT2 - PO$VMT1)/PO$VMT1
PO$VMT_23 <- (PO$VMT3 - PO$VMT2)/PO$VMT3

PO$VMT_12_g <- ifelse(PO$VMT_12 < -0.5, "[< -50%]",
                ifelse(PO$VMT_12 < -0.1, "[-50%,-10%]",
                ifelse(PO$VMT_12 < -0.05, "[-10%,-5%]",
                ifelse(PO$VMT_12 < 0.05, "[-5%,+5%]",
                ifelse(PO$VMT_12 < 0.1, "[+5%,+10%]",
                ifelse(PO$VMT_12 < 0.5, "[+10%,+50%]","[+50%]"))))))

PO$VMT_23_g <- ifelse(PO$VMT_23 < -0.5, "[< -50%]",
                ifelse(PO$VMT_23 < -0.1, "[-50%,-10%]",
                ifelse(PO$VMT_23 < -0.05, "[-10%,-5%]",
                ifelse(PO$VMT_23 < 0.05, "[-5%,+5%]",
                ifelse(PO$VMT_23 < 0.1, "[+5%,+10%]",
                ifelse(PO$VMT_23 < 0.5, "[+10%,+50%]","[+50%]"))))))



# creat point dataframe of segments
coords <- PO[ , c("lon", "lat")]   # coordinates
data   <- PO                      # data
crs    <- CRS("+proj=longlat +ellps=WGS84")  # proj4string of coords

# make the spatial points data frame object
POD <- SpatialPointsDataFrame(coords = coords,
                               data = data, 
                               proj4string = crs)
POD <- spTransform(POD, CRS=CRS(paste0("+init=epsg:22523")))
POD$id <- 1:7819
POB <- gBuffer(POD, byid=T, width = 200)

TRR <- readOGR(dsn="data/roads", layer="TR_roads_dissolved")
TRR <- spTransform(TRR, CRS=CRS(paste0("+init=epsg:22523")))

POD$dist_tr <- gDistance(POD, TRR, byid=T)[1,]
PO <- merge(PO, POD@data[,c("point_id", "dist_tr")], by="point_id")

POF <- fortify(POB)

POF <- merge(POF, POD, by="id")

p1 <- ggplot() +
  geom_polygon(aes(x = long, y = lat.x, group = id,
                   fill = VMT_12_g), data=POF) +
  scale_fill_manual(values = c("[< -50%]" = "#990000",
                               "[-50%,-10%]" = "#FF6666",
                               "[-10%,-5%]" = "#FFCCCC",
                               "[-5%,+5%]" = "#CCCCCC",
                               "[+5%,+10%]" = "#CCFFCC",
                               "[+10%,+50%]"  ="#669933",
                               "[> +50%]" = "#003300"),
                    limits = c("[< -50%]","[-50%,-10%]",
                               "[-10%,-5%]","[-5%,+5%]",
                               "[+5%,+10%]","[+10%,+50%]",
                               "[> +50%]"),
                    name = "VMT change\nper segment") +
  theme_void() +
  labs(title="Panel A: Simulated changes in VMT\nafter speed limit reductions of 2015")

p2 <- ggplot() +
  geom_polygon(aes(x = long, y = lat.x, group = id,
                   fill = VMT_23_g), data=POF) +
  scale_fill_manual(values = c("[< -50%]" = "#990000",
                               "[-50%,-10%]" = "#FF6666",
                               "[-10%,-5%]" = "#FFCCCC",
                               "[-5%,+5%]" = "#CCCCCC",
                               "[+5%,+10%]" = "#CCFFCC",
                               "[+10%,+50%]"  ="#669933",
                               "[> +50%]" = "#003300"),
                    limits = c("[< -50%]","[-50%,-10%]",
                               "[-10%,-5%]","[-5%,+5%]",
                               "[+5%,+10%]","[+10%,+50%]",
                               "[> +50%]"),
                    name = "S2 to S3\nVMT change") +
  theme_void() +
  labs(title="Panel B: Simulated changes in VMT\nafter speed limit reversal of 2017")

ml <- get_legend(p1)

pp1 <- p1 + guides(fill=F)
pp2 <- p2 + guides(fill=F)




map <- get_stamenmap(bbox=c(left=-46.85,
                            bottom=-23.7,
                            right = -46.4,
                            top = -23.4),
                     zoom = 10, maptype = "terrain-lines")

#i <- sample(1:15483,1)
i <- 4765
mr1 <- mt1[[i]]
mr1 <- spTransform(mr1, CRS("+proj=longlat +ellps=WGS84"))
mr2 <- mt2[[i]]
mr2 <- spTransform(mr2, CRS("+proj=longlat +ellps=WGS84"))
mr3 <- mt3[[i]]
mr3 <- spTransform(mr3, CRS("+proj=longlat +ellps=WGS84"))

r1 <- fortify(mr1)
r2 <- fortify(mr2)
r3 <- fortify(mr3)

p3 <- ggmap(map)+
  geom_path(aes(x=long,y=lat,colour="s1"),data=r1, size=2, alpha=0.5) +
  geom_path(aes(x=long,y=lat,colour="s2"),data=r2, size=1.5) +
  geom_label(aes(x=r1$long[1]+0.02,
                 y=r1$lat[1]), label="Origin") +
  geom_label(aes(x=r1$long[nrow(r1)],
                 y=r1$lat[nrow(r1)]-0.015), label="Destination") +
  theme_void() +
  scale_colour_manual(values = c("s1" = "navy",
                                 "s2" = "red"),
                      labels = c("s1" = "Before 2015 reductions",
                                 "s2" = "After 2015 reductions"),
                      limits = c("s1","s2"),
                    name = "Simulated routes:  ") +
  theme(legend.position = "top") +
  labs(title = "Panel A: Example of a trip with different simulated routes\nbefore and after the 2015 speed limit reductions")


 i <- 10618
 mr1 <- mt1[[i]]
 d1 <- gLength(mr1)
 mr1 <- spTransform(mr1, CRS("+proj=longlat +ellps=WGS84"))
 mr2 <- mt2[[i]]
 d2 <- gLength(mr2)
 mr2 <- spTransform(mr2, CRS("+proj=longlat +ellps=WGS84"))
 mr3 <- mt3[[i]]
 d3 <- gLength(mr3)
 mr3 <- spTransform(mr3, CRS("+proj=longlat +ellps=WGS84"))

#10618
#7136
#10449

r2 <- fortify(mr2, group=group)
r3 <- fortify(mr3, group=group)

p4 <- ggmap(map)+
  geom_path(aes(x=long,y=lat,colour="s2"),data=r2, size=2, alpha=0.5) +
  geom_path(aes(x=long,y=lat,colour="s3"),data=r3, size=1.5) +
  geom_label(aes(x=r2$long[1]+0.02,y=r2$lat[1]), label="Origin") +
  geom_label(aes(x=r2$long[nrow(r2)],
                 y=r2$lat[nrow(r2)]-0.015), label="Destination") +
  theme_void() +
  scale_colour_manual(values = c("s2" = "red",
                                 "s3" = "forestgreen"),
                      labels = c("s2" = "Before 2017 reversal",
                                 "s3" = "After 2017 reversal"),
                      limits = c("s2","s3"),
                      name = "Simulated routes:  ") +
  theme(legend.position = "top") +
  labs(title = "Panel B: Example of a trip with different simulated routes\nbefore and after the 2017 speed limit reversal")

ap1 <- plot_grid(pp1,NULL, pp2, NULL, ml,
                 nrow=1,ncol=5, rel_widths = c(0.9,0.1,0.9,0.1,0.25))
ap2 <- plot_grid(p3,NULL,p4,NULL,
                 nrow=1,ncol=4, rel_widths = c(1,0.1,1,0.1))
ap <- plot_grid(ap1,ap2,nrow=2,ncol=1)

pp <- plot_grid(p3,p4,nrow=2,ncol=1)
ggsave("simulation_plots.pdf",ap,  w=10,h=10)
ggsave("simulation_plots2.pdf",ap2,  w=10,h=5)