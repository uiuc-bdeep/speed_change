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

OP.p <- "appendix/tables/C5_cba_spillovers/A_cba_benefits_spill.csv"


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

# Define cohorts
PD$cohort <- PD$change_month_num - 24186
PD$seg <- 1
PD$cohort_sum <- ave(PD$seg, PD$cohort, FUN=sum)

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

### Create Relative month Covariates per cohort
for(c in 1:6){
 for(m in 0:17){
  PD[,paste0("c",c,"m",m)] <- ifelse(PD[,"cohort"] %in% c & PD[,"tr_dist"]%in% m, 1, 0)
 }
}

# exclude the first quarter before the Speed Limit Change Date (due to anticipation effects)
PD <- PD[!(PD$q_dist %in% c(-1)),]



# post treatment Arterials (used in the baseline model)
for(i in 0:5){PD[, paste0("Q",abs(i))] <- ifelse(PD[,"q_dist"] %in% i & PD[,"marg"] %in% 0, 1, 0)}
# post treatment Marginais (used in the baseline model)
for(i in 0:5){PD[, paste0("mQ",abs(i))] <- ifelse(PD[,"q_dist"] %in% i & PD[,"marg"] %in% 1, 1, 0)}

PD$mon <- as.character(substr(PD$date,1,7))


## Subset to segments with at least 1 accidents in the whole period
# recalculate total accidents in the remaining sample
PD$accidents_total <- ave(PD$accidents_m, PD$point_id, FUN=sum)
PD <- PD[PD$accidents_total >=1,]





#SPILLOVERS ----------------------------------------------------------

#define spillover areas
PD$spill <- ifelse(PD$tr_group %in% "co" & !is.na(PD$buff1600),1,0)
#PD$spill <- 0

# spillver post
PD$sQ0 <- ifelse(PD$spill %in% 1 & PD$date_m %in% as.Date(c("2016-01-01","2016-02-01","2016-03-01")), 1, 0)
PD$sQ1 <- ifelse(PD$spill %in% 1 & PD$date_m %in% as.Date(c("2016-04-01","2016-05-01","2016-06-01")), 1, 0)
PD$sQ2 <- ifelse(PD$spill %in% 1 & PD$date_m %in% as.Date(c("2016-07-01","2016-08-01","2016-09-01")), 1, 0)
PD$sQ3 <- ifelse(PD$spill %in% 1 & PD$date_m %in% as.Date(c("2016-10-01","2016-11-01","2016-12-01")), 1, 0)
PD$sQ4 <- ifelse(PD$spill %in% 1 & PD$date_m %in% as.Date(c("2017-01-01","2017-02-01","2017-03-01")), 1, 0)
PD$sQ5 <- ifelse(PD$spill %in% 1 & PD$date_m > as.Date("2017-03-30"), 1, 0)



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


# post treatment Arterials (used in the baseline model)
for(i in 0:5){PD[, paste0("Q",abs(i))] <- ifelse(PD[,"q_dist"] %in% i & PD[,"marg"] %in% 0, 1, 0)}
# post treatment Marginais (used in the baseline model)
for(i in 0:5){PD[, paste0("mQ",abs(i))] <- ifelse(PD[,"q_dist"] %in% i & PD[,"marg"] %in% 1, 1, 0)}

PD$mon <- as.character(substr(PD$date,1,7))




### Create Data for Each Model ------------------------------------------------

# Spilover segments
SP <- PD[PD$spill %in% 1,]
#SP$tr <- 0

# Model 0 - Treatment Only (Event Study)
ES <- PD[PD$tr_group %in% "tr" | PD$spill %in% 1,]

# Model 1 - all control segments (C1)
C1 <- PD[PD$tr_group %in% c("tr","co"),]

# MOdel 2 - with controls except segments within 1600m of treatment
C2 <- PD[PD$tr_group %in% "tr" | is.na(PD$buff1600),]

## MOdel 3 - with matched controls (also excludes segments within 1600m of treatment)
# segment level data
SD <- C2[!duplicated(C2$point_id),]
# treatment dummy
SD$tr <- ifelse(SD$tr_group %in% "tr",1,0)


# match based on accidents in the pre period
m.out <- matchit(tr ~ acc_pre_seg,
                 data = SD[,c("tr", "acc_pre_seg", "point_id")],
                 method = "nearest")
# get matching result
MS <- match.data(m.out)
# indentify matched segments
match_segs <- unique(MS$point_id)
C3 <- C2[C2$point_id %in% match_segs,]
C3 <- rbind(C3,SP)

# Regressions ----------------------------------------
c1m_t <- paste0("c1m",seq(0,17))
c2m_t <- paste0("c2m",seq(0,17))
c3m_t <- paste0("c3m",seq(0,17))
c4m_t <- paste0("c4m",seq(0,17))
c5m_t <- paste0("c5m",seq(0,17))
c6m_t <- paste0("c6m",seq(0,17))
f0 <- as.formula(paste("accidents_m ~ ",
                       paste(c1m_t, collapse= "+"),"+", paste(c2m_t, collapse= "+"),"+", 
                       paste(c3m_t, collapse= "+"),"+", paste(c4m_t, collapse= "+"),"+",
                       paste(c5m_t, collapse= "+"),"+", paste(c6m_t, collapse= "+"),"+",
                       "camera + camera_D + t + log_fuel + log_cameras + 
                       sQ0 + sQ1 + sQ2 + sQ3 + sQ4 + sQ5 | point_id"))
# Event Study
m0 <- femlm(f0,data = ES)

# With matched controls located more than 1.6km of treatment
m3 <- femlm(accidents_m ~ Q0 + Q1 + Q2 + Q3 + Q4 + Q5 +
             mQ0 + mQ1 + mQ2 + mQ3 + mQ4 +mQ5 +
             camera + camera_D  + sQ0 + sQ1 + sQ2 + sQ3 + sQ4 + sQ5 | point_id + mon,
            data = C3)

### BASELINE ACCIDENTS (Q-2, treated group only)
baseline <- sum(PD$accidents_m[(PD$q_dist %in% -2 & PD$tr_group %in% "tr") |
                               (PD$date_m %in% as.Date(c("2015-01-01","2015-02-01","2015-03-01")) &
                                PD$spill %in% 1) ])*4
baseline_marg <- sum(PD$accidents_m[(PD$q_dist %in% -2 & PD$tr_group %in% "tr" & PD$marg %in% 1) |
                                    (PD$date_m %in% as.Date(c("2015-01-01","2015-02-01","2015-03-01")) &
                                     PD$spill_marg %in% 1)])*4*sambd

### COUNTERFACTUALS (Q5, treated group only)

## without any change
WO <- PD[(PD$tr_group %in% "tr" & PD$q_dist %in% 5) |
         (PD$spill %in% 1 & PD$date_m %in% as.Date(c("2017-04-01","2017-05-01","2017-06-01"))),] 


### Zero out all relative month coefficients
for(c in 1:6){
 for(m in 0:17){
  WO[,paste0("c",c,"m",m)] <- 0
 }
}
### Zero out all relative month coefficients
for(q in 0:5){
 WO[,paste0("Q",q)] <- 0
 WO[,paste0("mQ",q)] <- 0
 WO[,paste0("sQ",q)] <- 0
}
WO$camera_D <- 0

# with change but no new cameras
WC <- PD[(PD$tr_group %in% "tr" & PD$q_dist %in% 5) |
          (PD$spill %in% 1 & PD$date_m %in% as.Date(c("2017-04-01","2017-05-01","2017-06-01"))),] 
WC$camera_D <- WC$camera14

## with change and with new cameras
WI <- PD[(PD$tr_group %in% "tr" & PD$q_dist %in% 5) |
          (PD$spill %in% 1 & PD$date_m %in% as.Date(c("2017-04-01","2017-05-01","2017-06-01"))),] 

# MArginais Only
WIM <- WI[WI$marg %in% 1 | WI$spill_marg %in% 1,]
WOM <- WO[WO$marg %in% 1 | WO$spill_marg %in% 1,] 
WCM <- WC[WC$marg %in% 1 | WC$spill_marg %in% 1,] 
### Zero out all arterial roads treatment coefficients
WIM$sQ5 <- ifelse(WIM$marg %in% 0, 1, 0)
WIM$Q5 <- 0
WCM$sQ5 <- ifelse(WCM$marg %in% 0, 1, 0)
WCM$Q5 <- 0

# create output table -A1
ot <- as.data.frame(matrix(nrow=6,ncol=0))

# scenario name
ot$scen <- c("without","SLR","SLR_cam",
             "without_marg","SLR_marg","SLR_cam_marg")

# observed accidents
ot$base <- c(rep(baseline, 3), rep(baseline_marg, 3))

# observed accidents
ot$obs <- c(sum(WO$accidents_m)*4,sum(WC$accidents_m)*4,sum(WI$accidents_m)*4,
            sum(WOM$accidents_m)*4*sambd,sum(WCM$accidents_m)*4*sambd,sum(WIM$accidents_m)*4*sambd)


# predict accidents in each cell - model 0 & 3
WO$yhat0 <- predict(m0, WO)
WO$yhat3 <- predict(m3, WO)

WC$yhat0 <- predict(m0, WC)
WC$yhat3 <- predict(m3, WC)

WI$yhat0 <- predict(m0, WI)
WI$yhat3 <- predict(m3, WI)


WOM$yhat0 <- predict(m0, WOM)
WOM$yhat3 <- predict(m3, WOM)

WCM$yhat0 <- predict(m0, WCM)
WCM$yhat3 <- predict(m3, WCM)

WIM$yhat0 <- predict(m0, WIM)
WIM$yhat3 <- predict(m3, WIM)

# predicted sums
ot$pred_m0 <- c(sum(WO$yhat0)*4,sum(WC$yhat0)*4,sum(WI$yhat0)*4,
                sum(WOM$yhat0)*4*sambd,sum(WCM$yhat0)*4*sambd,sum(WIM$yhat0)*4*sambd)
ot$pred_m3 <- c(sum(WO$yhat3)*4,sum(WC$yhat3)*4,sum(WI$yhat3)*4,
                sum(WOM$yhat3)*4*sambd,sum(WCM$yhat3)*4*sambd,sum(WIM$yhat3)*4*sambd)

# averted
ot$av_m0[1:3] <- ot$pred_m0[1]-ot$pred_m0[1:3]
ot$av_m3[1:3] <- ot$pred_m3[1]-ot$pred_m3[1:3]
ot$av_m0[4:6] <- ot$pred_m0[4]-ot$pred_m0[4:6]
ot$av_m3[4:6] <- ot$pred_m3[4]-ot$pred_m3[4:6]


# averted costs
ot$avc_m0 <- ot$av_m0*mca/1000000
ot$avc_m3 <- ot$av_m3*mca/1000000



sum(WI$yhat0[WI$spill%in%1])
sum(WO$yhat0[WO$spill%in%1])

sum(WIM$yhat0[WIM$spill_marg%in%1])
sum(WOM$yhat0[WOM$spill_marg%in%1])


fwrite(ot, OP.p,row.names = F)




