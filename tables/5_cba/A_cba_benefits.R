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

# inputs ----------------------------------------------------------------------
PD.p <- "data/panel/monthly_panel_all_b1k.rds"
ID.p <- "data/inflation/inflation.csv"
AD.p <- "data/accidents/accidents.rds"
HO.p <- "data/holidays/holidays_and_fare.csv"


# regressions
P1p <- "data/reg_out/baseline_data.rds"
ESp <- "data/reg_out/Event_Study_data.rds"
C3p <- "data/reg_out/Control_3_data.rds"
m0p <- "data/reg_out/Event_Study_model_CATT.rds"
m3p <- "data/reg_out/Control_3_model_3.rds"



# output

OP.p <- "tables/5_cba/A_cba_benefits.csv"


# prelim ----------------------------------------------------------------------------------------

# Calculate mean cost of accidents
ID <- read.csv(ID.p)
inflation <- ID$index[ID$date == as.character("2016-12-01")]/ID$index[ID$date == as.character("2014-12-01")]
AD <- readRDS(AD.p)
AD$cost <- AD$damages*inflation
mca <- mean(AD$cost, na.rm=T)
fpa <- sum(AD$mortos[AD$marg%in%1], na.rm = T)/nrow(AD[AD$marg%in%1,])

# Share of accidents in Marginais on business days
AM <- AD[AD$marg %in% 1 & year(AD$date)==2014,]
HO <- read.csv(HO.p)
HO$date <- as.Date(as.character(HO$DATE))
AM$date_d <- as.Date(as.character(AM$date))
AM <- merge(AM, HO[,c("date", "Holiday", "WeekDay")], by = "date", all.x = T)
AMBD <- AM[AM$Holiday %in% 0 & AM$WeekDay %in% c(2:6),]
sambd <- nrow(AMBD)/nrow(AM)

# read regressions
PD <- readRDS(P1p)
ES <- readRDS(ESp)
C3 <- readRDS(C3p)
m0 <- readRDS(m0p)
m3 <- readRDS(m3p)

 
### BASELINE ACCIDENTS (Q-2, treated group only)
baseline <- sum(PD$accidents_m[PD$q_dist %in% -2 & PD$tr_group %in% "tr"])*4
baseline_marg <- sum(PD$accidents_m[PD$q_dist %in% -2 & PD$tr_group %in% "tr" & PD$marg %in% 1])*4*sambd

### COUNTERFACTUALS (Q5, treated group only)

## without any change
WO <- PD[PD$tr_group %in% "tr" & PD$q_dist %in% 5,] 

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
}
WO$camera_D <- 0

# with change but no new cameras
WC <- PD[PD$tr_group %in% "tr" & PD$q_dist %in% 5,] 
WC$camera_D <- WC$camera14

## with change and with new cameras
WI <- PD[PD$tr_group %in% "tr" & PD$q_dist %in% 5,] 

# MArginais Only
WIM <- WI[WI$marg %in% 1,]
WOM <- WO[WO$marg %in% 1,] 
WCM <- WC[WC$marg %in% 1,] 



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



fwrite(ot, OP.p,row.names = F)




