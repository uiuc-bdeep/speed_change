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


# inputs ----------------------------------------------------------------------
PD.p <- "data/panel/monthly_panel_all.rds"

  # treatment buffers
TB.dsn <- "data/roads/speed_change_roads/buffers"
TB1.layer <- "treated_buffers_1000m"
TB2.layer <- "treated_buffers_2000m"

# output
OP.p <-"tables/scripts/accidents_regression/pooled_linear_relative/accidents_regression.csv"



# main ----------------------------------------------------------------------------------------
# read data
PD <- readRDS(PD.p)
PD <- as.data.frame(PD)

# subset to treated segments with more than 2 accidents
PD$tr_group <- ifelse(is.na(PD$ChangeDate), "co", "tr")
# exclude point NA
PD <- PD[!is.na(PD$point_id),]


# Find restricted control points (1km)
TB1 <- readOGR(dsn = TB.dsn, layer = TB1.layer)
TB2 <- readOGR(dsn = TB.dsn, layer = TB2.layer)
CP <- PD[!duplicated(PD$point_id),]
CP <- CP[CP$tr_group %in% "co",]
CP.sdf <- CP
coordinates(CP.sdf) <- ~lon+lat
proj4string(CP.sdf) <- CRS(paste0("+proj=longlat +ellps=WGS84"))
TB1 <- spTransform(TB1, CRS=CRS("+proj=longlat +ellps=WGS84"))
TB2 <- spTransform(TB2, CRS=CRS("+proj=longlat +ellps=WGS84"))
TB1@data$buff1 <- "1km"
TB2@data$buff2 <- "2km"
TB1 <- TB1[,7]
TB2 <- TB2[,7]
CP.sdf@data <- cbind(CP.sdf@data, over(CP.sdf, TB1))
CP.sdf@data <- cbind(CP.sdf@data, over(CP.sdf, TB2))
CPB <- CP.sdf@data
CPB <- CPB[,c("point_id", "buff1", "buff2")]
my.cp1 <- CPB$point_id[is.na(CPB$buff1)]
my.cp2 <- CPB$point_id[is.na(CPB$buff2)]


PD$pre <- ifelse(PD$date < as.Date("2015-07-01"), 1, 0)
PD$acc_pre <- PD$pre*PD$accidents_m
PD$acc_pre_seg <- ave(PD$acc_pre, PD$point_id, FUN=sum)

# prepare data
PD$Qpre <- ifelse(PD$q_dist <= -12, 1, 0)
PD$Qpos <- ifelse(PD$q_dist >= 6, 1, 0)
PD$Qpre[is.na(PD$Qpre)] <- 0
PD$Qpos[is.na(PD$Qpos)] <- 0

PD <- PD[PD$accidents_total >=2,]

# data for linear model
PD$acc_seg <- ave(PD$accidents_m, PD$point_id, FUN=mean)
PD$acc_rel <- PD$accidents_m/PD$acc_seg


for(i in -11:-1){PD[, paste0("Q_",abs(i))] <- ifelse(PD[,"q_dist"] %in% i, 1, 0)}
for(i in 0:5){PD[, paste0("Q",abs(i))] <- ifelse(PD[,"q_dist"] %in% i, 1, 0)}
PD$mon <- as.character(substr(PD$date,1,7))

PD <- PD[!(PD$tr_dist %in% c(-1,-2)),]

# matched
SD <- PD[!duplicated(PD$point_id),]
SD$tr <- ifelse(SD$tr_group == "tr",1,0)
m.out <- matchit(tr ~ acc_pre_seg,
                 data = SD[,c("tr", "acc_pre_seg", "point_id")],
                 method = "nearest")
MS <- match.data(m.out)
match_segs <- unique(MS$point_id)
MSS <- MS[!duplicated(MS$point_id),c("point_id", "weights")]
MD <- PD[PD$point_id %in% match_segs,]
MD <- merge(MD, MSS, by = "point_id", all.x = T)


# matched - threshold distance
SD2 <- PD[!duplicated(PD$point_id),]
SD2 <- SD2[SD2$tr_group %in% "tr" | SD2$point_id %in% my.cp1,]
SD2$tr <- ifelse(SD2$tr_group == "tr",1,0)
m.out2 <- matchit(tr ~ acc_pre_seg,
                 data = SD2[,c("tr", "acc_pre_seg", "point_id")],
                 method = "nearest")
MS2 <- match.data(m.out2)
match_segs2 <- unique(MS2$point_id)
MSS2 <- MS2[!duplicated(MS2$point_id),c("point_id", "weights")]
MD2 <- PD[PD$point_id %in% match_segs2,]
MD2 <- merge(MD2, MSS2, by = "point_id", all.x = T)


# no controls
NC <- PD[PD$tr_group %in% "tr",]


# Linear Model
PD2 <- PD[PD$accidents_total >= 20,]

m0 <- felm(acc_rel ~ Qpre + Q_11 + Q_10 + Q_9 + Q_8 + Q_7 + Q_6 + Q_5 + Q_4 + Q_3 + Q_1 +
                     Q0 + Q1 + Q2 + Q3 + Q4 + Q5 + Qpos +
                     camera_B + camera_D + log_fuel + log_cameras + t | point_id, data = NC, weights = NC$accidents_total)

m1 <- felm(acc_rel ~ Qpre + Q_11 + Q_10 + Q_9 + Q_8 + Q_7 + Q_6 + Q_5 + Q_4 + Q_3 + Q_1 +
                     Q0 + Q1 + Q2 + Q3 + Q4 + Q5 + Qpos +
                     camera_B + camera_D| point_id + mon, data = PD, weights = PD$accidents_total)

m2 <- felm(acc_rel ~ Qpre + Q_11 + Q_10 + Q_9 + Q_8 + Q_7 + Q_6 + Q_5 + Q_4 + Q_3 + Q_1 +
                     Q0 + Q1 + Q2 + Q3 + Q4 + Q5 + Qpos +
                     camera_B + camera_D| point_id + mon, data = PD2, weights = PD2$accidents_total)

m3 <- felm(acc_rel ~ Qpre + Q_11 + Q_10 + Q_9 + Q_8 + Q_7 + Q_6 + Q_5 + Q_4 + Q_3 + Q_1 +
                     Q0 + Q1 + Q2 + Q3 + Q4 + Q5 + Qpos +
                     camera_B + camera_D| point_id + mon, data = MD, weights = MD$accidents_total)

m4 <- felm(acc_rel ~ Qpre + Q_11 + Q_10 + Q_9 + Q_8 + Q_7 + Q_6 + Q_5 + Q_4 + Q_3 + Q_1 +
                     Q0 + Q1 + Q2 + Q3 + Q4 + Q5 + Qpos +
                     camera_B + camera_D| point_id + mon, data = MD2, weights = MD2$accidents_total)


a0 <- as.data.frame(summary(m0)$coefficients)
a1 <- as.data.frame(summary(m1)$coefficients)
a2 <- as.data.frame(summary(m2)$coefficients)
a3 <- as.data.frame(summary(m3)$coefficients)
a4 <- as.data.frame(summary(m4)$coefficients)

a0$var <- rownames(a0)
a1$var <- rownames(a1)
a2$var <- rownames(a2)
a3$var <- rownames(a3)
a4$var <- rownames(a4)

a0$model <- "0"
a1$model <- "1"
a2$model <- "2"
a3$model <- "3"
a4$model <- "4"

a0$obs <- m0$N
a1$obs <- m1$N
a2$obs <- m2$N
a3$obs <- m3$N
a4$obs <- m4$N

a0$r2 <- summary(m0)$adj.r.squared
a1$r2 <- summary(m1)$adj.r.squared
a2$r2 <- summary(m2)$adj.r.squared
a3$r2 <- summary(m3)$adj.r.squared
a4$r2 <- summary(m4)$adj.r.squared

rownames(a0) <- paste(a0$var, a0$model)
rownames(a1) <- paste(a1$var, a1$model)
rownames(a2) <- paste(a2$var, a2$model)
rownames(a3) <- paste(a3$var, a3$model)
rownames(a4) <- paste(a4$var, a4$model)

a <- rbind(a0, a1,a2, a3, a4)

a$star <- ifelse(a$`Pr(>|t|)` < 0.001, "***",
          ifelse(a$`Pr(>|t|)` < 0.01, "**",
          ifelse(a$`Pr(>|t|)` < 0.05, "*"," ")))

write.csv(a, OP.p)