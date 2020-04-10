# clear memory
rm(list=ls())
gc()

# required packages
library(ggplot2)
library(Cairo)
library(scales)
library(cowplot)
library(rgdal)
library(data.table)


# inputs ----------------------------------------------------------------------
TD.p <- "data/tickets/all_tickets.rds"

# Road Segments Buffers (RB)
RB.dsn <- "data/roads/speed_change_roads"
RB.layer <- "treated_buffers_200m"


# output
o1 <- "appendix/tables/A4_tickets_descriptives/tickets_descriptives.csv"


# main ----------------------------------------------------------------------------------------
# read data
TD <- readRDS(TD.p)


# buffers
RB <- readOGR(dsn = RB.dsn, layer = RB.layer)


# convert tickets to cameras
CD <- TD[!duplicated(TD$place.id),]

CD <- as.data.frame(CD)

CDsp <- CD[!is.na(CD$lat3) & !is.na(CD$long3),]
coordinates(CDsp) <- ~long3+lat3
proj4string(CDsp) <- CRS("+proj=longlat +ellps=WGS84")

# match projection
RB <- spTransform(RB, CRS=CRS("+proj=longlat +ellps=WGS84"))

CDsp@data <- cbind(CDsp@data, over(CDsp, RB))
CDsp@data$lat <- CDsp@coords[,2]
CDsp@data$lon <- CDsp@coords[,1]

CDG <- CDsp@data
CDG <- CDG[,c("place.id", "StreetName")]
CDG <- as.data.table(CDG)
TD <- merge(TD, CDG, by = "place.id", all.x = T)

# year
TD$tic <- TD$tickets
TD$dri <- TD$tickets*TD$ticket.rodizio
TD$sp0 <- TD$tickets*TD$ticket.speed
TD$sp1 <- TD$tickets*TD$ticket.speed_0
TD$sp2 <- TD$tickets*TD$ticket.speed_20
TD$sp3 <- TD$tickets*TD$ticket.speed_50
TD$out <- TD$tic - TD$dri - TD$sp1 - TD$sp2 - TD$sp3

TD$marg <- ifelse(TD$StreetName %in% c("AV MARGINAL DO RIO PINHEIROS",
                                         "AV MARGINAL DO RIO TIETE"),1,0)

# my output table
mo <- as.data.frame(matrix(nrow = 10, ncol = 5))
colnames(mo) <- c("All_Years", "Y2014", "Y2015", "Y2016", "Y2017")
rownames(mo) <- c("tic_all", "dri_all", "sp0_all", "out_all",
                  "tic_marg", "dri_marg", "sp0_marg", "out_marg")

my.vars <- c("tic", "dri", "sp0", "out")

TD <- as.data.frame(TD)

for( i in 1:length(my.vars)){
 #i <- 1
mo[i,] <- c(sum(TD[,my.vars[i] ] ), 
            sum(TD[TD$year %in% 2014 ,my.vars[i] ]),
            sum(TD[TD$year %in% 2015 ,my.vars[i] ]),
            sum(TD[TD$year %in% 2016 ,my.vars[i] ]),
            sum(TD[TD$year %in% 2017 ,my.vars[i] ]))
}

MD <- TD[TD$marg %in% 1,]
for( i in 1:length(my.vars)){
#i <- 1
mo[i+4,] <- c(sum(MD[,my.vars[i] ] ), 
            sum(MD[MD$year %in% 2014 ,my.vars[i] ]),
            sum(MD[MD$year %in% 2015 ,my.vars[i] ]),
            sum(MD[MD$year %in% 2016 ,my.vars[i] ]),
            sum(MD[MD$year %in% 2017 ,my.vars[i] ]))
}

# cameras

TD$place.id_month <- paste(TD$place.id, TD$year_month)
PT <- TD[!duplicated(TD$place.id_month),]
PT$cam <- 1
PT$cams <- ave(PT$cam, PT$year_month, FUN=sum)
PT$cams_marg <- ave(PT$marg, PT$year_month, FUN=sum)

tot_cam <- max(PT$cams)
tot_shr <- 1
tot_cam14 <- max(PT$cams[PT$year %in% 2014])
tot_cam15 <- max(PT$cams[PT$year %in% 2015])
tot_cam16 <- max(PT$cams[PT$year %in% 2016])
tot_cam17 <- max(PT$cams[PT$year %in% 2017])

marg_cam <- max(PT$cams_marg)
marg_shr <- marg_cam/tot_cam
marg_cam14 <- max(PT$cams_marg[PT$year %in% 2014])
marg_cam15 <- max(PT$cams_marg[PT$year %in% 2015])
marg_cam16 <- max(PT$cams_marg[PT$year %in% 2016])
marg_cam17 <- max(PT$cams_marg[PT$year %in% 2017])

mo[9,] <- c(tot_cam, tot_cam14, tot_cam15, tot_cam16, tot_cam17)
mo[10,] <- c(marg_cam, marg_cam14, marg_cam15, marg_cam16, marg_cam17)
write.csv(mo, o1)


