
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
library(Cairo)
library(stringr)
library(rgdal)
library(rgeos)

# inputs ----------------------------------------------------------------------
PD.p <- "data/panel/monthly_panel_all_b1k.rds"
ID.p <- "data/inflation/inflation.csv"
AD.p <- "data/accidents/accidents.rds"
HO.p <- "data/holidays/holidays_and_fare.csv"

# output
OP.p <- "appendix/tables/C5_cba_spillovers/A2_cba_benefits_spill_CATT.csv"
         

# prelim ----------------------------------------------------------------------------------------




# Calculate mean cost of accidents
ID <- read.csv(ID.p)
inflation <- ID$index[ID$date == as.character("2016-12-01")]/ID$index[ID$date == as.character("2014-12-01")]
AD <- readRDS(AD.p)
AD$cost <- AD$damages*inflation
mca <- mean(AD$cost, na.rm=T)



# Share of accidents in Marginais on business days

AM <- AD[AD$marg %in% 1 & year(AD$date)==2014,]
HO <- read.csv(HO.p)
HO$date <- as.Date(as.character(HO$DATE))
AM$date_d <- as.Date(as.character(AM$date))
AM <- merge(AM, HO[,c("date", "Holiday", "WeekDay")], by = "date", all.x = T)
AMBD <- AM[AM$Holiday %in% 0 & AM$WeekDay %in% c(2:6),]
sambd <- nrow(AMBD)/nrow(AM)

# read data
PD <- readRDS(PD.p)

# define tr and co groups
PD$tr_group <- ifelse(is.na(PD$ChangeDate), "co", "tr")

# exclude point NA
PD <- PD[!is.na(PD$point_id),]

# count accidents in the pre period by segment (used for matching)
PD$pre <- ifelse(PD$date < as.Date("2015-07-01"), 1, 0)
PD$acc_pre <- PD$pre*PD$accidents_m
PD$acc_pre_seg <- ave(PD$acc_pre, PD$point_id, FUN=sum)

# drop anything above the 18th relative quarter in TR group
PD <- PD[PD$tr_dist <=17 | PD$tr_group %in% "co",]

# calendar month
PD$mon <- as.character(substr(PD$date,1,7))


#SPILLOVERS ----------------------------------------------------------
#define spillover areas
PD$spill <- ifelse(PD$tr_group %in% "co" & !is.na(PD$buff1600),1,0)
#PD$spill <- 0

# spillver post
PD$sQ0 <- ifelse(PD$date_m %in% as.Date(c("2016-01-01","2016-02-01","2016-03-01")), 1, 0)
PD$sQ1 <- ifelse(PD$date_m %in% as.Date(c("2016-04-01","2016-05-01","2016-06-01")), 1, 0)
PD$sQ2 <- ifelse(PD$date_m %in% as.Date(c("2016-07-01","2016-08-01","2016-09-01")), 1, 0)
PD$sQ3 <- ifelse(PD$date_m %in% as.Date(c("2016-10-01","2016-11-01","2016-12-01")), 1, 0)
PD$sQ4 <- ifelse(PD$date_m %in% as.Date(c("2017-01-01","2017-02-01","2017-03-01")), 1, 0)
PD$sQ5 <- ifelse(PD$date_m > as.Date("2017-03-30"), 1, 0)



# SPILLOVER MARGINAIS---------------------------------------------------
MA <- readOGR(dsn="data/roads", layer="buffer_1600")
MA <- spTransform(MA, CRS("+proj=longlat +datum=WGS84"))

PPD <- PD[!duplicated(PD$point_id),c("point_id", "lon", "lat")]
PP <- PPD
coordinates(PP) <- ~lon+lat
PP@data <- merge(PP@data, PPD, by="point_id", all.x=T)
proj4string(PP) <- CRS("+proj=longlat +datum=WGS84")

PPO <- over(PP,MA)
colnames(PPO) <- "marg_buffer"
PP@data <- cbind(PP@data, PPO)
PPDD <- PP@data[,c("point_id","marg_buffer")]

PD <- merge(PD, PPDD, by="point_id", all.x=T)
# define new spillover are
PD$spill_marg <- ifelse(!is.na(PD$marg_buffer) & !(PD$marg %in% 1),1,0)
#PD$spill_marg <- 0

### Create Data for Each Model ------------------------------------------------

# Spilover segments
SP <- PD[PD$spill %in% 1,]


# remove the treatment period
SP <- SP[!(SP$mon %in% c("2015-07", "2015-08","2015-09",
                         "2015-10", "2015-11","2015-12")),]

# Marginais Spillover segments
MSP <- PD[PD$spill_marg %in% 1,]
MSP <- MSP[MSP$tr_group %in% "tr",]

SP <- rbind(SP, MSP)

## Subset to segments with at least 1 accidents in the whole period
# recalculate total accidents in the remaining sample
SP$accidents_total <- ave(SP$accidents_m, SP$point_id, FUN=sum)
SP <- SP[SP$accidents_total >=1,]


# Regressions ----------------------------------------
# Event Study
m0 <- femlm(accidents_m ~ camera + t + log_fuel + log_cameras + 
                       sQ0 + sQ1 + sQ2 + sQ3 + sQ4 + sQ5 | point_id,data = SP)
# With matched controls located more than 1.6km of treatment
#mm0 <- femlm(accidents_m ~ camera + t + log_fuel + log_cameras + 
#                       sQ0 + sQ1 + sQ2 + sQ3 + sQ4 + sQ5 | point_id,data = MSP)

summary(m0)

### COUNTERFACTUALS (Q5, treated group only)

## without any change
WO <- SP[SP$date_m %in% as.Date(c("2017-04-01","2017-05-01","2017-06-01")),] 

### Zero out all relative month coefficients
for(q in 0:5){
 WO[,paste0("sQ",q)] <- 0
}

## with change and with new cameras
WI <- SP[SP$date_m %in% as.Date(c("2017-04-01","2017-05-01","2017-06-01")),] 

# MArginais Only
WIM <- WI[WI$spill_marg %in% 1,]
WOM <- WO[WO$spill_marg %in% 1,] 

# create output table -A1
ot <- as.data.frame(matrix(nrow=4,ncol=0))

# scenario name
ot$scen <- c("without","SLR",
             "without_marg","SLR_marg")

# observed accidents

# observed accidents
ot$obs <- c(sum(WO$accidents_m)*4,sum(WI$accidents_m)*4,
            sum(WOM$accidents_m)*4*sambd,sum(WIM$accidents_m)*4*sambd)

# predict accidents in each cell - model 0 & 3
WO$yhat0 <- predict(m0, WO)
WI$yhat0 <- predict(m0, WI)

WOM$yhat0 <- predict(m0, WOM)
WIM$yhat0 <- predict(m0, WIM)

# predicted sums
ot$pred_m0 <- c(sum(WO$yhat0)*4,sum(WI$yhat0)*4,
                sum(WOM$yhat0)*4*sambd,sum(WIM$yhat0)*4*sambd)

# averted
ot$av_m0[1:2] <- ot$pred_m0[1]-ot$pred_m0[1:2]
ot$av_m0[3:4] <- ot$pred_m0[3]-ot$pred_m0[3:4]

# averted costs
ot$avc_m0 <- ot$av_m0*mca/1000000

fwrite(ot, file=OP.p,row.names = F)




