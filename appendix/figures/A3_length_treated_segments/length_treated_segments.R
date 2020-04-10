# clear memory
rm(list=ls())
gc()

#required packages
library(ggplot2)
library(Cairo)
library(ggmap)
library(rgdal)
library(ggsn)
library(scales)


# inputs 
  # roads
  roads.dsn <- "data/roads/speed_change_roads"
  roads.path <- "speed_change_roads"
  

# output
OD.p <- "appendix/figures/A3_length_treated_segments/length_treated_segments.pdf"


# Main ----------------------------------------------------------------------------------------


# Roads
  # read shapes
  RD <- readOGR(roads.dsn, roads.path)

  # remove duplicated roads
  RD <- RD[!(RD@data$Direction %in% c("North", "West")),]
  
  # convert projection to meters
  RD <- spTransform(RD, CRS=CRS(paste0("+proj=merc +lon_0=0 ",
                                       "+lat_ts=6.644567463596037 ",
                                       "+x_0=0 +y_0=0 +ellps=GRS80 ",
                                       "+units=m +no_defs")))
  # calculate lengths
  RD@data$length <- SpatialLinesLengths(RD)
  
  # Total Length
  TOT.L <- sum(RD@data$length)
  
  # convert to data.frame
  RDF <- RD@data
  
  # calculate cumulative treated length
  RDF$ctl <- 0
  
  # fix change date
  RDF$ChangeDate <- as.Date(gsub("/", "-", as.character(RDF$ChangeDate)))
  
  for(i in 1:nrow(RDF)){
   # i <- 1
    RDF$ctl[i] <- sum(RDF$length[RDF$ChangeDate <= RDF$ChangeDate[i]])
  }
  
  RDFP <- RDF[!duplicated(RDF$ChangeDate),c("ChangeDate", "ctl")]
  RDFP0 <- as.data.frame(matrix(nrow = 2, ncol = 0))
  RDFP0$ChangeDate <- c(as.Date("2015-07-01"), as.Date("2016-01-01"))
  RDFP0$ctl <- c(0, max(RDFP$ctl))
  
  RDFP1 <- rbind(RDFP, RDFP0)
  
  RDFP1$ctl <- RDFP1$ctl/1000
  
  ggplot(RDFP1) +
   geom_step(aes(x=ChangeDate, y = ctl)) +
   theme_bw() +
   scale_x_date(limits = c(as.Date("2015-07-01"), as.Date("2016-01-01")),
                breaks = date_breaks("1 month"),
                labels = date_format("%b-%Y")) +
   scale_y_continuous(breaks = seq(0, 600, 200), limits = c(0,600), label = comma) +
   theme(panel.grid = element_blank() ) +
   labs(y = "Cumulative Length (Km)\n",
        x = "Date\n")
  ggsave(OD.p, w = 6, h = 4)
  
  
  
