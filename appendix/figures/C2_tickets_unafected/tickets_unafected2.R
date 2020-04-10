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
library(rgeos)
library(rgdal)


# inputs ----------------------------------------------------------------------
TD.p <- "data/tickets/tickets_over_segments_all.rds"

CR.dsn <- "data/roads/speed_change_roads/buffers"
CR1600.lay <- "treated_buffers_1600"
CR200.lay <- "treated_buffers_200"

PD.p <- "data/panel/monthly_panel_all_b1k.rds"


# output
OP.p <-"figures/C2_tickets_unafected/tickets_es2.pdf"

# main ----------------------------------------------------------------------------------------

# read tickets data
TI <- readRDS(TD.p)
PD <- readRDS(PD.p)

# keep only other types of tickets
TI <- TI[TI$ticket.speed%in% 1,]
TI$date_m <- as.Date(paste0(year(TI$date),"-",
                            month(TI$date),"-01"))
TI$loc_date <- paste(TI$Local, TI$date)

# get camera min and max date
#TI$Local<- as.character(TI$point_id)
TI$min_date <- ave(TI$date, TI$Local, FUN=min)
TI$max_date <- ave(TI$date, TI$Local, FUN=max)

# keep only the cameras that were in place in 2014
#TI <- TI[TI$min_date < as.Date("2014-07-01"),]


# read segment buffers
TB16 <- readOGR(dsn=CR.dsn, layer=CR1600.lay)
TB2 <- readOGR(dsn=CR.dsn, layer=CR200.lay)
# cameras
MC <- TI[!duplicated(TI$Local),]
MCXY <- MC[,c("long3", "lat3")]
MCSP <- SpatialPointsDataFrame(coords = MCXY, data = MC,
        proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
TB16 <- spTransform(TB16, CRS = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
TB2 <- spTransform(TB2, CRS = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# CO far
CF <- MCSP[is.na(over(MCSP, TB16)$point_id),]
# CO close
CC <- MCSP[is.na(over(MCSP, TB2)$point_id) &
           !is.na(over(MCSP, TB16)$point_id) ,]
# TR
TC <- MCSP[!is.na(over(MCSP, TB2)$point_id),]


#aggregate tickets per camera_date
TI$tic_d <- ave(TI$tickets, TI$loc_date, FUN=sum)
#daily data
LM<-TI[!duplicated(TI$loc_date),]

# take log of tickets
LM$log_tic <- log(LM$tic_d)

# absolute quarter
for(y in 2014:2017){
 LM[,paste0("ym_",y,"_1")] <- ifelse(year(LM[,"date"])%in%y & month(LM[,"date"])%in%c(1:3),1,0)
 LM[,paste0("ym_",y,"_2")] <- ifelse(year(LM[,"date"])%in%y & month(LM[,"date"])%in%c(4:6),1,0)
 LM[,paste0("ym_",y,"_3")] <- ifelse(year(LM[,"date"])%in%y & month(LM[,"date"])%in%c(7:9),1,0)
 LM[,paste0("ym_",y,"_4")] <- ifelse(year(LM[,"date"])%in%y & month(LM[,"date"])%in%c(10:12),1,0)
}


LM$t <- as.numeric(LM$date)
LM <- LM[LM$date >= as.Date("2015-01-01"),]

# get panel covariates
PD2 <- PD[!duplicated(PD$date_m),c("date_m", "log_fuel", "log_cameras")]
LM <- merge(LM, PD2, by="date_m",all.x=T)
PD3 <- PD[!duplicated(PD$point_id),c("point_id", "ChangeDate")]
LM <- merge(LM, PD3, by="point_id",all.x=T)

#LM <- LM[LM$min_date < as.Date("2014-07-01"),]
#LM <- LM[LM$max_date >= as.Date("2017-12-01"),]

# relative data
# get change date
LM$date_diff <- as.numeric(LM$date - LM$ChangeDate)
LM$date_diff_q <- floor(LM$date_diff/90)+4
summary(LM$date_diff_q)
for(y in 0:13){
 LM[,paste0("ymr_",y)] <- ifelse(LM$date_diff_q %in%y,1,0)
}


# weights
LM$w <- ave(LM$tic_d, LM$Local, FUN=mean)

# tickets data
TI2 <- LM[LM$Local%in%unique(CF$Local),]
TI1 <- LM[LM$Local%in%unique(CC$Local),]
TI0 <- LM[!is.na(LM$date_diff_q),]
TIX <- TI0

summary(TI0$date[TI0$ym_2015_2%in%1])
http://rsvieira.com/files/papers/Vieira_2018_fare_free_elder.pdf
http://rsvieira.com/files/papers/Ang_Christensen_Vieira_2019_speed_limit.pdf
http://rsvieira.com/files/papers/Vieira_and_Arends_Kuenning_2019_affirmative_action.pdf

m0 <- felm(log_tic~
   ym_2015_1+
            ym_2015_3+ym_2015_4+
            ym_2016_1+ym_2016_2+ym_2016_3+ym_2016_4+
            ym_2017_1+ym_2017_2+ym_2017_3+ym_2017_4+
            log_cameras + log_fuel|Local|0|date,data=TI0,
           weights=TI0$w)
S0 <- as.data.frame(summary(m0)$coefficients)


m1 <- felm(log_tic~           ym_2015_1+

            ym_2015_3+ym_2015_4+
            ym_2016_1+ym_2016_2+ym_2016_3+ym_2016_4+
            ym_2017_1+ym_2017_2+ym_2017_3+ym_2017_4+
            log_cameras + log_fuel|Local|0|date,data=TI1,
           weights=TI1$w)
S1 <- as.data.frame(summary(m1)$coefficients)


m2 <- felm(log_tic~ ym_2015_1+

            ym_2015_3+ym_2015_4+
            ym_2016_1+ym_2016_2+ym_2016_3+ym_2016_4+
            ym_2017_1+ym_2017_2+ym_2017_3+ym_2017_4+
            log_cameras + log_fuel|Local|0|date,data=TI2,
           weights=TI2$w)
S2 <- as.data.frame(summary(m2)$coefficients)


TIX <- TIX[TIX$date_diff_q %in% c(2:11),]
summary(TIX$date_diff_q)
mx <- felm(log_tic~ymr_2+ymr_4+ymr_5+
            ymr_6+ymr_7+ymr_8+ymr_9+ymr_10+ymr_11+
            log_cameras + log_fuel|Local|0|date,data=TIX,
           weights=TIX$w)
Sx <- as.data.frame(summary(mx)$coefficients)




s0 <- S0[1:11,]
s0$m <- as.Date(paste0(substr(row.names(s0),4,7),"-",
        3*as.numeric(substr(row.names(s0),9,9))-1,"-10"))
ss0 <- s0[1,]
ss0$Estimate <- 0
ss0$`Cluster s.e.` <- 0
ss0$m <- as.Date("2015-05-10")
s0 <- rbind(s0,ss0)
s0$mod <- "s0"

s1 <- S1[1:11,]
s1$m <- as.Date(paste0(substr(row.names(s1),4,7),"-",
                       3*as.numeric(substr(row.names(s1),9,9))-1,"-15"))
ss0 <- s1[1,]
ss0$Estimate <- 0
ss0$`Cluster s.e.` <- 0
ss0$m <- as.Date("2015-05-15")
s1 <- rbind(s1,ss0)
s1$mod <- "s1"

s2 <- S2[1:11,]
s2$m <- as.Date(paste0(substr(row.names(s2),4,7),"-",
                       3*as.numeric(substr(row.names(s2),9,9))-1,"-20"))
ss0 <- s2[1,]
ss0$Estimate <- 0
ss0$`Cluster s.e.` <- 0
ss0$m <- as.Date("2015-05-20")
s2 <- rbind(s2,ss0)
s2$mod <- "s2"


sx <- Sx[1:9,]
sx$m <- as.numeric(substr(row.names(sx),5,6))-4
ssx <- sx[1,]
ssx$Estimate <- 0
ssx$`Cluster s.e.` <- 0
ssx$m <- -1
sx <- rbind(sx,ssx)
sx$mod <- "sx"


ss <- rbind(s0,s1,s2)


ss$up <- ss$Estimate + 1.96*ss$`Cluster s.e.`
ss$do <- ss$Estimate - 1.96*ss$`Cluster s.e.`

sx$up <- sx$Estimate + 1.96*sx$`Cluster s.e.`
sx$do <- sx$Estimate - 1.96*sx$`Cluster s.e.`

# slr
sb <- as.data.frame(matrix(nrow = 1, ncol = 0))
sb$x0 <- as.Date("2015-07-20")
sb$xf <- as.Date("2015-12-30")
sb$y0 <- -Inf
sb$yf <- Inf


my_p0 <- ggplot() +
 geom_hline(yintercept = 0, alpha=0.5) +
 geom_rect(aes(xmin = x0, xmax = xf,
               ymin = y0, ymax = yf,
               fill = "sb"), alpha = 0.05, data = sb) +
 geom_vline(aes(linetype="sli",
            xintercept = as.Date("2017-01-24")),alpha=0.5) +
 geom_vline(aes(linetype="slr1",
            xintercept = as.Date("2015-07-20")),alpha=0.5) +
 geom_vline(aes(linetype="slr2",
            xintercept = as.Date("2015-10-01")),alpha=0.5) +
 geom_vline(aes(linetype="slr3",
            xintercept = as.Date("2016-01-01")),alpha=0.5) +
 geom_point(aes(x=m,y=Estimate, colour=mod),data=ss[ss$mod=="s0",]) +
 geom_errorbar(aes(x=m,ymin=do,ymax=up, colour=mod),data=ss[ss$mod=="s0",], width=0) +
 theme_bw() +
 theme(panel.grid = element_blank()) +
 scale_y_continuous(label=percent, breaks = seq(-1,2,0.5)) +
 coord_cartesian(ylim=c(-1,2)) +
 labs(title = "Panel A: tickets from cameras on treated roads",
      y = "changes in tickets",
      x = "date") +
 scale_fill_manual(values = c("sb" = "red"),
                   labels = c("sb" = "\nspeed limit\nreductions\n"),
                   name = "") + 
 scale_colour_manual(values = c("s0" = "#FF3333",
                                "s1" = "#999999",
                                "s2" = "#222222"),
                     labels = c("s0" = "\ncameras on treated roads\n",
                                "s1" = "cameras near treated roads\n(<1600m)\n",
                                "s2" = "cameras far from treated roads\n(>1600m)"),
                     name = "") + 
 scale_linetype_manual(limits = c("slr1","slr2","slr3","sli"),
                       values = c("sli" = 2,"slr1" = 3,
                                  "slr2" = 4,"slr3" = 5),
                       labels = c("sli" = "\nspeed limit reversal\n",
                                  "slr1" = "\nbegining of speed limit reductions\n",
                                  "slr2" = "speed limit reductions:\n (48% of road segments treated)\n",
                                  "slr3" = "speed limit reductions:\n (100% of road segments treated)\n"),
                       name = "") + 
 scale_x_date(limits = c(as.Date("2014-12-01"), as.Date("2018-01-01")),
              breaks = date_breaks("3 month"),
              labels = date_format("%b\n%Y"))  +
 theme(legend.position = "top", legend.direction = "horizontal",
       legend.key = element_rect(size = 12),
       legend.key.height = unit(1.5, "lines"),
       legend.title = element_text(hjust = 0.5)) +
 guides(colour = FALSE, fill = FALSE, linetype=F)




my_p1 <- ggplot() +
 geom_hline(yintercept = 0, alpha=0.5) +
 geom_rect(aes(xmin = x0, xmax = xf,
               ymin = y0, ymax = yf,
               fill = "sb"), alpha = 0.05, data = sb) +
 geom_vline(aes(linetype="sli",
            xintercept = as.Date("2017-01-24")),alpha=0.5) +
 geom_vline(aes(linetype="slr1",
            xintercept = as.Date("2015-07-20")),alpha=0.5) +
 geom_vline(aes(linetype="slr2",
            xintercept = as.Date("2015-10-01")),alpha=0.5) +
 geom_vline(aes(linetype="slr3",
            xintercept = as.Date("2016-01-01")),alpha=0.5) +
 geom_point(aes(x=m,y=Estimate, colour=mod),data=ss[ss$mod=="s1",]) +
 geom_errorbar(aes(x=m,ymin=do,ymax=up, colour=mod),data=ss[ss$mod=="s1",], width=0) +
 theme_bw() +
 theme(panel.grid = element_blank()) +
 scale_y_continuous(label=percent, breaks = seq(-1,2,0.5)) +
 coord_cartesian(ylim=c(-1,2)) +
 labs(title = "Panel B: tickets from cameras near treated roads (<1600m)",
      y = "changes in tickets",
      x = "date") +
 scale_fill_manual(values = c("sb" = "red"),
                   labels = c("sb" = "\nspeed limit\nreductions\n"),
                   name = "") + 
 scale_colour_manual(values = c("s0" = "#FF3333",
                                "s1" = "#999999",
                                "s2" = "#222222"),
                     labels = c("s0" = "\ncameras on treated roads\n",
                                "s1" = "cameras near treated roads\n(<1600m)\n",
                                "s2" = "cameras far from treated roads\n(>1600m)"),
                     name = "") + 
 scale_linetype_manual(limits = c("slr1","slr2","slr3","sli"),
                       values = c("sli" = 2,"slr1" = 3,
                                  "slr2" = 4,"slr3" = 5),
                       labels = c("sli" = "\nspeed limit reversal\n",
                                  "slr1" = "\nbegining of speed limit reductions\n",
                                  "slr2" = "speed limit reductions:\n (48% of road segments treated)\n",
                                  "slr3" = "speed limit reductions:\n (100% of road segments treated)\n"),
                       name = "") + 
 scale_x_date(limits = c(as.Date("2014-12-01"), as.Date("2018-01-01")),
              breaks = date_breaks("3 month"),
              labels = date_format("%b\n%Y"))  +
 theme(legend.position = "top", legend.direction = "horizontal",
       legend.key = element_rect(size = 12),
       legend.key.height = unit(1.5, "lines"),
       legend.title = element_text(hjust = 0.5)) +
 guides(colour = FALSE, fill = FALSE, linetype=F)






my_p2 <- ggplot() +
 geom_hline(yintercept = 0, alpha=0.5) +
 geom_rect(aes(xmin = x0, xmax = xf,
               ymin = y0, ymax = yf,
               fill = "sb"), alpha = 0.05, data = sb) +
 geom_vline(aes(linetype="sli",
            xintercept = as.Date("2017-01-24")),alpha=0.5) +
 geom_vline(aes(linetype="slr1",
            xintercept = as.Date("2015-07-20")),alpha=0.5) +
 geom_vline(aes(linetype="slr2",
            xintercept = as.Date("2015-10-01")),alpha=0.5) +
 geom_vline(aes(linetype="slr3",
            xintercept = as.Date("2016-01-01")),alpha=0.5) +
 geom_point(aes(x=m,y=Estimate, colour=mod),data=ss[ss$mod=="s2",]) +
 geom_errorbar(aes(x=m,ymin=do,ymax=up, colour=mod),data=ss[ss$mod=="s2",], width=0) +
 theme_bw() +
 theme(panel.grid = element_blank()) +
 scale_y_continuous(label=percent, breaks = seq(-1,2,0.5)) +
 coord_cartesian(ylim=c(-1,2)) +
 labs(title = "Panel C: tickets from cameras far from treated roads (>1600m)",
      y = "changes in tickets",
      x = "date") +
 scale_fill_manual(values = c("sb" = "red"),
                   labels = c("sb" = "\nspeed limit\nreductions\n"),
                   name = "") + 
 scale_colour_manual(values = c("s0" = "#FF3333",
                                "s1" = "#999999",
                                "s2" = "#222222"),
                     labels = c("s0" = "\ncameras on treated roads\n",
                                "s1" = "cameras near treated roads\n(<1600m)\n",
                                "s2" = "cameras far from treated roads\n(>1600m)"),
                     name = "") + 
 scale_linetype_manual(limits = c("slr1","slr2","slr3","sli"),
                       values = c("sli" = 2,"slr1" = 3,
                                  "slr2" = 4,"slr3" = 5),
                       labels = c("sli" = "\nspeed limit reversal\n",
                                  "slr1" = "\nbegining of speed limit reductions\n",
                                  "slr2" = "speed limit reductions:\n (48% of road segments treated)\n",
                                  "slr3" = "speed limit reductions:\n (100% of road segments treated)\n"),
                       name = "") + 
 scale_x_date(limits = c(as.Date("2014-12-01"), as.Date("2018-01-01")),
              breaks = date_breaks("3 month"),
              labels = date_format("%b\n%Y"))  +
 theme(legend.position = "top", legend.direction = "horizontal",
       legend.key = element_rect(size = 12),
       legend.key.height = unit(1.5, "lines"),
       legend.title = element_text(hjust = 0.5)) +
 guides(colour = FALSE, fill = FALSE, linetype=F)
my_p2






p1 <- ggplot() +
 geom_hline(yintercept = 0, alpha=0.5) +
 geom_rect(aes(xmin = x0, xmax = xf,
               ymin = y0, ymax = yf,
               fill = "sb"), alpha = 0.05, data = sb) +
 geom_vline(aes(linetype="sli",
            xintercept = as.Date("2017-01-24")),alpha=0.5) +
 geom_vline(aes(linetype="slr1",
            xintercept = as.Date("2015-07-20")),alpha=0.5) +
 geom_vline(aes(linetype="slr2",
            xintercept = as.Date("2015-10-01")),alpha=0.5) +
 geom_vline(aes(linetype="slr3",
            xintercept = as.Date("2016-01-01")),alpha=0.5) +
 scale_fill_manual(values = c("sb" = "red"),
                   labels = c("sb" = "speed limit\nreductions"),
                   name = "") + 
 scale_linetype_manual(limits = c("slr1","slr2","slr3","sli"),
                       values = c("sli" = 2,"slr1" = 3,
                                  "slr2" = 4,"slr3" = 5),
                       labels = c("sli" = "speed limit\nreversal",
                                  "slr1" = "begining of\nspeed limit reductions",
                                  "slr2" = "speed limit reductions:\n (48% of road segments treated)",
                                  "slr3" = "speed limit reductions:\n (100% of road segments treated)"),
                       name = "") +
  theme_bw()+
 theme(legend.position = "top", legend.direction = "horizontal") +
 guides(fill = guide_legend(order = 1),
        linetype = guide_legend(order = 2))

l1 <- get_legend(p1)


ap2 <- plot_grid(l1,my_p0,my_p1,my_p2, nrow = 4, ncol=1,
                rel_heights = c(0.1,1,1,1))
ap2
ggsave(OPp,ap2,w=12,h=12)




