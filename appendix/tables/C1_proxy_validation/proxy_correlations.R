
# clear memory
rm(list=ls())
gc()

# required packages
library(lfe)
library(ggplot2)
library(data.table)
library(scales)
library(cowplot)
library(rgdal)
library(rgeos)

# inputs ----------------------------------------------------------------------
AT <- readRDS("data/tickets/tickets_over_segments_all.rds")
PD <- readRDS("data/panel/monthly_panel_all_b1k.rds")
VM <- readRDS("data/osrm_simulations/segments_VMT.rds")
KM <- readRDS("data/congestion/kms_cet_reghod.rds")


# output
OPP <- "appendix/tables/C1_proxy_validation/proxy_correlations_2"
#############################################
# VMT Validation
#############################################


VM$VMT3[VM$VMT3==0] <- 400

PD <- merge(PD, VM, by="point_id",all.x=T)

# subset to August-2017
AT$obs <- 1
TI <- AT[AT$date >= as.Date("2017-01-01") &
         AT$date <= as.Date("2017-12-31") ,] 

# subset to weekdays
#TI<-TI[TI$day %in%c(1:4,7:11,14:18,21:25,28:31),]

# separate to ticket type
OT <- TI[TI$ticket.other%in%1,]
RE <- TI[TI$ticket.rodizio%in%1,]
SP <- TI[TI$ticket.speed%in%1,]

# count tickets per segment
SP$tics_sp <- SP$tickets*SP$ticket.speed
RE$tics_re <- RE$tickets*RE$ticket.rodizio
OT$tics_ot <- OT$tickets*OT$ticket.other

# tickets per segment
SP$tics_sp_m <- ave(SP$tics_sp, SP$point_id, FUN=sum)
RE$tics_re_m <- ave(RE$tics_re, RE$point_id, FUN=sum)
OT$tics_ot_m <- ave(OT$tics_ot, OT$point_id, FUN=sum)

# point_id level data
SC <- SP[!duplicated(SP$point_id),]
RC <- RE[!duplicated(RE$point_id),]
OC <- OT[!duplicated(OT$point_id),]

# log tickets
SC$log_sp <- log(SC$tics_sp_m)
RC$log_re <- log(RC$tics_re_m)
OC$log_ot <- log(OC$tics_ot_m)


# add VMT change info
PP <- PD[!duplicated(PD$point_id),c("point_id", "VMT3", "lat", "lon")]
PP <- merge(PP, SC[!duplicated(SC$point_id),c("point_id","tics_sp_m", "log_sp")],
            by="point_id", all.x=T)
PP <- merge(PP, RC[!duplicated(RC$point_id),c("point_id","tics_re_m", "log_re")],
            by="point_id", all.x=T)
PP <- merge(PP, OC[!duplicated(OC$point_id),c("point_id","tics_ot_m", "log_ot")],
            by="point_id", all.x=T)
PP$log_vmt <- log(PP$VMT3)


m1 <- felm(log_re~log_vmt,data=PP)
s1 <- as.data.frame(summary(m1)$coefficients)
s1$model <- "restriction_vmt"
s1$n <- m1$N

m2 <- felm(log_ot~log_vmt,data=PP)
s2 <- as.data.frame(summary(m2)$coefficients)
s2$model <- "other_vmt"
s2$n <- m2$N

#############################################
# Congestion Validation
#############################################

# get regions to each ticket
ZS <- readOGR(dsn="data/city_area/macrozones_9", layer="ZS")
ZS <- spTransform(ZS, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
ZS$reg <- "SUL"

ZL <- readOGR(dsn="data/city_area/macrozones_9", layer="ZL")
ZL <- spTransform(ZL, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
ZL$reg <- "LESTE"

ZN <- readOGR(dsn="data/city_area/macrozones_9", layer="ZN")
ZN <- spTransform(ZN, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
ZN$reg <- "NORTE"

ZO <- readOGR(dsn="data/city_area/macrozones_9", layer="ZO")
ZO <- spTransform(ZO, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
ZO$reg <- "OESTE"

CE <- readOGR(dsn="data/city_area/macrozones_9", layer="CE")
CE <- spTransform(CE, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
CE$reg <- "CENTRO"

R9 <- rbind(ZL,ZN,ZS,ZO,CE)

xy <- PP[,c("lon","lat")]
SS <- SpatialPointsDataFrame(coords = xy, data = PP,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
PP$reg <- over(SS,R9[,"reg"])$reg
RR <- PP[,c("point_id","reg")]


TI <- merge(TI, RR, by="point_id",all.x=T)

# separate to ticket type
OT <- TI[TI$ticket.other%in%1,]
RE <- TI[TI$ticket.rodizio%in%1,]
SP <- TI[TI$ticket.speed%in%1,]

# count tickets 
SP$tics_sp <- SP$tickets*SP$ticket.speed
RE$tics_re <- RE$tickets*RE$ticket.rodizio
OT$tics_ot <- OT$tickets*OT$ticket.other

# region date
SP$reg_h <- paste(SP$reg, SP$date)
RE$reg_h <- paste(RE$reg, RE$date)
OT$reg_h <- paste(OT$reg, OT$date)

# tickets per region per day
SP$tics_sp_m <- ave(SP$tics_sp, SP$reg_h, FUN=sum)
RE$tics_re_m <- ave(RE$tics_re, RE$reg_h, FUN=sum)
OT$tics_ot_m <- ave(OT$tics_ot, OT$reg_h, FUN=sum)

# date_reg level data
SC <- SP[!duplicated(SP$reg_h),]
RC <- RE[!duplicated(RE$reg_h),]
OC <- OT[!duplicated(OT$reg_h),]

# log tickets
SC$log_sp <- log(SC$tics_sp_m)
RC$log_re <- log(RC$tics_re_m)
OC$log_ot <- log(OC$tics_ot_m)

# congestion per date region
KM$date <- as.Date(KM$date)
KM$reg_h <- paste(KM$reg, KM$date)
KM$cong <- ave(KM$cet_m, KM$reg_h, FUN=mean)

KK <- KM[!duplicated(KM$reg_h),c("reg_h","cong","reg","date")]
KK <- KK[KK$date >= as.Date("2017-01-01") &
         KK$date <= as.Date("2017-12-31") ,] 
KK <- merge(KK,SC[,c("reg_h","log_sp","tics_sp_m")], by="reg_h", all.x=T)
KK <- merge(KK,RC[,c("reg_h","log_re","tics_re_m")], by="reg_h", all.x=T)
KK <- merge(KK,OC[,c("reg_h","log_ot","tics_ot_m")], by="reg_h", all.x=T)
KK$log_cong <- log(KK$cong)

m3 <- felm(log_re~log_cong|reg,data=KK)
s3 <- as.data.frame(summary(m3)$coefficients)
s3$model <- "restriction_congestion"
s3$n <- m3$N

m4 <- felm(log_ot~log_cong|reg,data=KK)
s4 <- as.data.frame(summary(m4)$coefficients)
s4$model <- "other_congestion"
s4$n <- m4$N

summary(KK$log_cong)
KK$log_cong_q1 <- ifelse(KK$log_cong<6.733,KK$log_cong,0)
KK$log_cong_q2 <- ifelse(KK$log_cong>=6.733 & KK$log_cong<7.095,KK$log_cong,0)
KK$log_cong_q3 <- ifelse(KK$log_cong>=7.095 & KK$log_cong<7.366,KK$log_cong,0)
KK$log_cong_q4 <- ifelse(KK$log_cong>=7.366,KK$log_cong,0)


m5 <- felm(log_re~log_cong_q1+log_cong_q2+log_cong_q3+log_cong_q4|reg,data=KK)
s5 <- as.data.frame(summary(m5)$coefficients)
s5$model <- "restriction_congestion_bins"
s5$n <- m5$N

m6 <- felm(log_ot~log_cong_q1+log_cong_q2+log_cong_q3+log_cong_q4|reg,data=KK)
s6 <- as.data.frame(summary(m6)$coefficients)
s6$model <- "other_congestion_bins"
s6$n <- m6$N



ss <- rbind(s1,s2,s3,s4,s5,s6)
ss$star <- ifelse(ss$`Pr(>|t|)`<0.001,"***",
           ifelse(ss$`Pr(>|t|)`<0.01,"**",
           ifelse(ss$`Pr(>|t|)`<0.05,"*"," ")))
write.csv(ss, OPP)
