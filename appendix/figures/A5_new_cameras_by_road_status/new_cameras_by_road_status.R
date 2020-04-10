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
  # cameras
  TD.p <- "data/tickets/tickets_over_segments.rds"


# output
OT.p <- "appendix/figures/A5_new_cameras_by_road_status/new_cameras_by_road_status.csv"
         

# Main ----------------------------------------------------------------------------------------

# Cameras

  # read tickets
  TD <- readRDS(TD.p)
  # subset to speeding tickets
  TD <- TD[TD$ticket.speed %in% 1,]

  # aggregate at camera level (finding minimum date of each camera, and keeping lat longs)
  TD$min.date <- as.Date(ave(as.numeric(TD$date), TD$place.id, FUN=min), origin = "1970-01-01")
  TD$max.date <- as.Date(ave(as.numeric(TD$date), TD$place.id, FUN=max), origin = "1970-01-01")
  CD <- TD[!duplicated(TD$place.id), c("place.id", "min.date", "max.date", "lat3", "long3","tr_group")]
  CD$tr_group <- ifelse(CD$tr_group %in% "tr", "tr", "co")
  
  # installation group
  CD$date.group <- ifelse(CD$min.date >= as.Date("2015-07-20"), "after", "before")
  
  # subset to cameras that were active in January 2015 and any date beyond
  CD <- CD[CD$max.date >= as.Date("2015-06-01"),]
  CDaf <- CD[CD$date.group == "after",]
  CDbf <- CD[CD$date.group == "before",]
  
  # create output matrix (OT)
  OT <- matrix(nrow = 2, ncol = 5)
  
  OT[1,1] <- nrow(CDbf[CDbf$tr_group == "tr",])
  OT[2,1] <- nrow(CDbf[CDbf$tr_group == "co",])
  
  OT[1,2] <- OT[1,1]/(OT[1,1] + OT[2,1])
  OT[2,2] <- OT[2,1]/(OT[1,1] + OT[2,1])
  
  OT[1,4] <- nrow(CDaf[CDaf$tr_group == "tr",])
  OT[2,4] <- nrow(CDaf[CDaf$tr_group == "co",])
  
  OT[1,5] <- OT[1,4]/(OT[1,4] + OT[2,4])
  OT[2,5] <- OT[2,4]/(OT[1,4] + OT[2,4])
  
  OT[1,3] <- ""
  OT[2,3] <- ""
  
  
  # save output
  write.csv(OT, OT.p)
  
  
  