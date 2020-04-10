#     ---------------------------------------------------------------------------------------- 
#   |  estimate the effect of treatent using difference in difference method                  |
#   |                                                                                         |
#   |  By:  Renato Vieira                                                                     |
#   |       Big Data for Environmental Economics and Policy                                   |
#   |       University of Illinois at Urbana Chamapaign                                       |
#     ---------------------------------------------------------------------------------------- 

# Prelims -------------------------------------------------------------------------------------

# clear memory
rm(list=ls())
gc()

# required packages
library(lfe)
library(stargazer)
library(ggplot2)
library(FENmlm)
library(msm)
library(zoo)
library(data.table)
library(scales)
library(cowplot)
library(MatchIt)
library(optmatch)
library(rgdal)
library(rgeos)

# inputs ----------------------------------------------------------------------
PD.p <- "data/panel/monthly_panel_all.rds"

  # treatment buffers
TB.dsn <- "data/roads/speed_change_roads/buffers"
TB.layer <- "treated_buffers_"

# output
OP.p <-"data/panel/monthly_panel_all_b1k.rds"



# main ----------------------------------------------------------------------------------------
# read data
PD <- readRDS(PD.p)
PD <- as.data.frame(PD)

for(i in seq(200,3000,200)){
# Find restricted control points (1km)
 #i<-200
 TB1 <- readOGR(dsn = TB.dsn, layer = paste0(TB.layer, i))
 CP <- PD[!duplicated(PD$point_id),]
 CP.sdf <- CP
 coordinates(CP.sdf) <- ~lon+lat
 proj4string(CP.sdf) <- CRS(paste0("+proj=longlat +ellps=WGS84"))
 TB1 <- spTransform(TB1, CRS=CRS("+proj=longlat +ellps=WGS84"))
 TB1@data[,paste0("buff",i)] <- paste0(i,"m")
 TB1 <- TB1[,7]
 CP.sdf@data <- cbind(CP.sdf@data, over(CP.sdf, TB1))
 CPB <- CP.sdf@data
 CPB <- CPB[,c("point_id", paste0("buff",i))]
 PD <- as.data.table(PD)
 CPB <- as.data.table(CPB)
 PD <- merge(PD, CPB, by = "point_id", all.x=T)
}

PD <- as.data.frame(PD)
saveRDS(PD, OP.p)