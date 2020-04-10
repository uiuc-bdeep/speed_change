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
AP <- readRDS("tables/2_accidents_estimation/reg_out/baseline_data.rds")

PD <- AP[!duplicated(AP$point_id),]
coords <- PD[ , c("lon", "lat")]   # coordinates
data   <- PD[ , c("point_id", "lon", "lat", "tr_group",
                  "accidents_total", "acc_pre",
                  "StreetName")]          # data
crs    <- CRS("+proj=longlat +ellps=WGS84") # proj4string of coords

# make the spatial points data frame object
SP0 <- SpatialPointsDataFrame(coords = coords,
                            data = data, 
                               proj4string = crs)

# project for distance in meters
SP <- spTransform(SP0,CRS("+init=epsg:22523"))
# fix variables
SP$id_800 <- 0
SP$StreetName <- as.character(SP$StreetName)
SP$tr_group <- as.character(SP$tr_group)

# create list for points with no pairs
NP <- list()
np_c <- 1

# create list for matched points
MP <- list()
mp_c <- 1

c <- 1
while(i>1){#i<-10
 
 # get initial segment
 P0 <- SP[1,]
 my_street <- P0$StreetName
 my_tr <- P0$tr_group

 # get remaining segments
 SPT <- SP[2:nrow(SP),]
 #calculate distance to all remaining segments
 SPT$temp_dist <- as.numeric(gDistance(P0,SPT,byid=T)[,1])

 # check if there exists a point with same street and tr_group and <400m
 P1 <- SPT[SPT$StreetName%in%my_street & SPT$tr_group %in% my_tr & SPT$temp_dist < 400,]
 if(nrow(P1)==0){
   NP[[np_c]] <- P0
   np_c <- np_c+1
   SP <- SPT
 }else{
   P1 <- P1[P1$temp_dist==min(P1$temp_dist),]
   P0$temp_dist <- min(P1$temp_dist)
   PP <- rbind(P0,P1)
   MP[[mp_c]] <- PP
   mp_c <- mp_c+1
   SP <- SPT[!(SPT$point_id%in%PP$point_id),]
 }
 cat(c, " ")
 c <- c+1
 i <- nrow(SP)
}

P0 <- SP[1,]
NP[[np_c]] <- P0


# merge all Matched points
for(i in 1:length(MP)){ MP[[i]]$id800 <- i}
for(i in 1:length(NP)){ NP[[i]]$id800 <- length(MP) + i}

# rbind all points
MM <- do.call(rbind, MP)
NN <- do.call(rbind, NP)

MN <- rbind(MM, NN)
writeOGR(MN, dsn="appendix/tables/B4_segment_length/_scripts", layer="id_800",
         driver = "ESRI Shapefile")
MND <- MN@data
MND$id800 <- as.character(MND$id800)
saveRDS(MND[,c("point_id","id800")], "appendix/tables/B4_segment_length/_scripts/id_800.rds")